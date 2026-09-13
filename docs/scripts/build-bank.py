"""Compile questions.org and contrats.org without evaluating Babel.
Run --check to reject stale outputs without rewriting any file.
"""
import argparse
from dataclasses import dataclass, field
import hashlib
import json
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[2]
STATES = {'[X]': True, '[ ]': False, '[?]': 'discussable', '—': None}
LINK = re.compile(r'\[\[([^\]]+)\](?:\[([^\]]*)\])?\]')


@dataclass
class Node:
    title: str
    level: int
    line: int
    tags: list = field(default_factory=list)
    props: dict = field(default_factory=dict)
    body: list = field(default_factory=list)
    children: list = field(default_factory=list)


def require(condition, message):
    if not condition:
        raise ValueError(message)


def trim_blank(lines):
    lines = list(lines)
    while lines and not lines[0].strip():
        lines.pop(0)
    while lines and not lines[-1].strip():
        lines.pop()
    return '\n'.join(lines)


def parse_org(text):
    root = Node('', 0, 0)
    stack, block, drawer = [root], None, False
    for number, line in enumerate(text.splitlines(), 1):
        stripped, node = line.strip(), stack[-1]
        if block:
            node.body.append(line)
            if stripped.lower() == '#+end_' + block:
                block = None
            continue
        begin = re.fullmatch(r'#\+begin_(\w+)(?:\s+.*)?', stripped, re.I)
        if begin:
            block = begin[1].lower()
            node.body.append(line)
            continue
        require(not stripped.lower().startswith('#+end_'), f'Ligne {number}: fin de bloc sans début')
        if stripped == ':PROPERTIES:':
            require(not drawer and not node.props, f'Ligne {number}: propriétés répétées')
            drawer = True
            continue
        if drawer:
            if stripped == ':END:':
                drawer = False
                continue
            match = re.fullmatch(r':([A-Z_]+):\s*(.*)', stripped)
            require(match and match[1] not in node.props, f'Ligne {number}: propriété invalide ou dupliquée')
            node.props[match[1]] = match[2]
            continue
        heading = re.fullmatch(r'(\*+) (.+)', line)
        if heading:
            level, title = len(heading[1]), heading[2]
            tags = re.search(r'\s+(:[\w:@#%]+:)\s*$', title)
            if tags:
                title = title[:tags.start()].rstrip()
            while stack[-1].level >= level:
                stack.pop()
            require(level == stack[-1].level + 1, f'Ligne {number}: niveau de titre sauté')
            child = Node(title, level, number, tags[1].strip(':').split(':') if tags else [])
            stack[-1].children.append(child)
            stack.append(child)
        else:
            node.body.append(line)
    require(not block and not drawer, 'Bloc Org ou tiroir de propriétés non fermé')
    return root


def properties(node, mandatory, optional=()):
    require(set(mandatory) <= node.props.keys(), f'{node.title}: propriétés requises {mandatory}')
    require(node.props.keys() <= set(mandatory) | set(optional), f'{node.title}: propriété inconnue')
    require(all(node.props[k] for k in mandatory), f'{node.title}: propriété vide')


def sections(node, names):
    require(not trim_blank(node.body), f'{node.title}: texte hors des sections prévues')
    result = {child.title: child for child in node.children}
    require(len(result) == len(node.children) and set(result) == set(names),
            f'{node.title}: sections attendues {names}, trouvées {list(result)}')
    return result


def verse(node):
    require(not node.children, f'{node.title}: sous-titre inattendu')
    text = trim_blank(node.body)
    match = re.fullmatch(r'#\+begin_verse\n(.*?)\n#\+end_verse', text, re.I | re.S)
    require(match and match[1].strip(), f'{node.title}: un bloc verse non vide est requis')
    require('\t' not in match[1], f'{node.title}: indenter avec des espaces, pas des tabulations')
    return match[1]  # Leading whitespace is an experimental variable.


def linked_code(cell):
    match = re.fullmatch(r'\[\[file:contrats.org::#([A-Z]+)\]\[\1\]\]', cell)
    require(match, f'Lien de contrat attendu, reçu : {cell}')
    return match[1]


def local_id(cell):
    match = re.fullmatch(r'\[\[#(R\d{2}-[1-9]\d*)\]\[\1\]\]', cell)
    require(match, f'Lien de rédaction attendu, reçu : {cell}')
    return match[1]


def table(node, name):
    lines = [line.strip() for line in node.body if line.strip()]
    require(lines and lines.pop(0) == '#+name: ' + name, f'{node.title}: table nommée {name} attendue')
    rows = []
    for line in lines:
        require(line.startswith('|') and line.endswith('|'), f'{name}: ligne hors tableau')
        if re.fullmatch(r'[|+\-\s]+', line):
            continue
        rows.append([cell.strip() for cell in line[1:-1].split('|')])
    require(rows and all(len(row) == len(rows[0]) for row in rows), f'{name}: tableau irrégulier')
    return rows


def read_contracts(document):
    contracts = {}
    for node in document.children:
        if node.title == 'Mode d’emploi':
            continue
        properties(node, ['CUSTOM_ID', 'TYPE', 'LIBELLE'])
        code = node.props['CUSTOM_ID']
        require(re.fullmatch('[A-Z]+', code) and code not in contracts, f'Code dupliqué ou invalide : {code}')
        require(node.title.startswith(code + ' — '), f'{code}: titre incohérent')
        require(node.props['TYPE'] in ('contrat', 'controle'), f'{code}: TYPE inconnu')
        sub = sections(node, ['Définition et codage', 'Appuis bibliographiques', 'Rédactions liées'])
        definition = trim_blank(sub['Définition et codage'].body)
        sources = trim_blank(sub['Appuis bibliographiques'].body)
        require(len(definition) > 60 and '[[docview:../bib/' in sources, f'{code}: définition ou référence paginée absente')
        require(not any(s.children for s in sub.values()), f'{code}: sous-section inattendue')
        contracts[code] = dict(label=node.props['LIBELLE'], kind=node.props['TYPE'], definition=definition)
    require(contracts, 'Référentiel vide')
    return contracts


def read_questions(document, contracts):
    meta = {}
    for line in document.body:
        match = re.fullmatch(r'#\+(BANK_VERSION|BANK_STATUS):\s*(.+)', line)
        if match:
            require(match[1] not in meta, 'Métadonnée dupliquée : ' + match[1])
            meta[match[1]] = match[2]
    require('BANK_VERSION' in meta and 'BANK_STATUS' in meta, 'Version ou statut de banque absent')
    require(re.fullmatch(r'\d+\.\d+\.\d+', meta['BANK_VERSION']), 'Version attendue : majeur.mineur.correctif')
    bank = dict(version=meta['BANK_VERSION'], status=meta['BANK_STATUS'], questions=[])
    all_ids = set()
    for node in document.children:
        if node.title == 'Mode d’emploi':
            continue
        properties(node, ['CUSTOM_ID', 'NIVEAU', 'DOMAINE', 'FAMILLE'])
        qid = node.props['CUSTOM_ID']
        require(re.fullmatch(r'R\d{2}', qid) and qid not in all_ids, f'Question dupliquée ou invalide : {qid}')
        require(node.title.startswith(qid + ' — '), f'{qid}: titre incohérent')
        require(node.props['NIVEAU'] in ['5e', '4e', '3e', '2de', '1re spé', 'Tle spé', 'Sup 1'], f'{qid}: niveau inconnu')
        require(re.fullmatch(r'R\d{2}', node.props['FAMILLE']), f'{qid}: famille non opaque')
        all_ids.add(qid)
        sub = sections(node, ['Énoncé', 'Réponse de référence', 'Contrats examinés', 'Rédactions'])
        q = dict(id=qid, level=node.props['NIVEAU'], domain=node.props['DOMAINE'],
                 title=node.title.split(' — ', 1)[1], statement=verse(sub['Énoncé']),
                 referenceAnswer=verse(sub['Réponse de référence']), family=node.props['FAMILLE'],
                 tags=node.tags, productions=[])
        rows = table(sub['Contrats examinés'], 'contrats-' + qid)
        require(rows[0][0] == 'Rédaction', f'{qid}: première colonne = Rédaction')
        columns = [linked_code(c) for c in rows[0][1:]]
        require(columns and len(set(columns)) == len(columns), f'{qid}: colonnes absentes ou dupliquées')
        require(all(c in contracts and contracts[c]['kind'] == 'contrat' for c in columns), f'{qid}: colonne inconnue ou contrôle utilisé comme contrat')
        checks = {}
        for row in rows[1:]:
            pid = local_id(row[0])
            require(pid not in checks, f'{qid}: ligne de codage dupliquée : {pid}')
            require(all(c in STATES for c in row[1:]), f'{pid}: cases permises : {list(STATES)}')
            checks[pid] = dict(zip(columns, (STATES[c] for c in row[1:])))
        for production in sub['Rédactions'].children:
            properties(production, ['CUSTOM_ID', 'CIBLES'], ['INDENTATION_DE'])
            pid = production.props['CUSTOM_ID']
            require(re.fullmatch(qid + r'-[1-9]\d*', pid) and pid not in all_ids, f'Rédaction dupliquée ou invalide : {pid}')
            require(production.title == pid, f'{pid}: titre incohérent')
            all_ids.add(pid)
            details = sections(production, ['Texte', 'Analyse'])
            value = production.props['CIBLES']
            targets = [linked_code(m[0]) for m in LINK.finditer(value)]
            require(not LINK.sub('', value).strip() and targets and len(set(targets)) == len(targets), f'{pid}: CIBLES invalides')
            require(all(c in contracts for c in targets), f'{pid}: cible inconnue')
            require(pid in checks, f'{pid}: ligne absente du tableau')
            require(all(c in checks[pid] and checks[pid][c] is not None for c in targets if contracts[c]['kind'] == 'contrat'), f'{pid}: contrat ciblé absent ou non pertinent dans la table')
            analysis = trim_blank(details['Analyse'].body)
            require(len(analysis) > 15 and not details['Analyse'].children, f'{pid}: analyse absente')
            research = dict(targets=targets, analysis=analysis, contracts=checks[pid])
            if 'INDENTATION_DE' in production.props:
                research['indentationOf'] = local_id(production.props['INDENTATION_DE'])
            q['productions'].append(dict(id=pid, content=verse(details['Texte']), research=research))
        require(len(q['productions']) >= 2, f'{qid}: au moins deux rédactions requises')
        require(set(checks) == {p['id'] for p in q['productions']}, f'{qid}: lignes de codage orphelines')
        bank['questions'].append(q)
    require(bank['questions'], 'Banque vide')
    for q in bank['questions']:
        by_id = {p['id']: p for p in q['productions']}
        for p in q['productions']:
            base_id = p['research'].get('indentationOf')
            if base_id:
                require(base_id in by_id and base_id != p['id'], f'{p["id"]}: paire hors question ou autoréférence')
                base = by_id[base_id]
                require('indentationOf' not in base['research'], f'{p["id"]}: paire chaînée')
                normalized = lambda text: [line.lstrip(' ') for line in text.split('\n')]
                require(normalized(p['content']) == normalized(base['content']) and p['content'] != base['content'], f'{p["id"]}: la paire doit différer uniquement par les retraits')
                require('SCOPE' in p['research']['targets'] and 'SCOPE' in base['research']['targets'], f'{p["id"]}: paire sans cible SCOPE')
    return bank


def check_links(path, text, virtual_files):
    pdf_pages = {}
    for link in LINK.finditer(text):
        target = link[1]
        if target.startswith('#'):
            file, anchor = path, target
        elif target.startswith(('file:', 'docview:')):
            filename, _, anchor = target.split(':', 1)[1].partition('::')
            require(not Path(filename).is_absolute(), f'{path.name}: lien absolu : {target}')
            file = (path.parent / filename).resolve()
            require(file.is_relative_to(ROOT), f'{path.name}: lien sortant du dépôt : {target}')
        else:
            continue
        require(file in virtual_files or file.exists(), f'{path.name}: fichier absent : {target}')
        if anchor.startswith('#'):
            source = virtual_files.get(file)
            if source is None:
                source = file.read_text(encoding='utf-8')
            require(re.search(r'^\s*:CUSTOM_ID:\s+' + re.escape(anchor[1:]) + r'\s*$', source, re.M), f'{path.name}: ancre absente : {target}')
        elif file.suffix.lower() == '.pdf':
            require((anchor.isdigit() and int(anchor) > 0) if target.startswith('docview:') else not anchor,
                    f'{path.name}: page PDF invalide : {target}')
            if anchor:
                if file not in pdf_pages:
                    info = subprocess.run(['pdfinfo', str(file)], capture_output=True, text=True, check=True).stdout
                    count = re.search(r'^Pages:\s+(\d+)', info, re.M)
                    require(count, f'Pagination PDF illisible : {file.name}')
                    pdf_pages[file] = int(count[1])
                require(int(anchor) <= pdf_pages[file], f'{path.name}: page hors PDF : {target}')


def org_table(headers, rows):
    return '\n'.join(['| ' + ' | '.join(headers) + ' |', '|' + '+'.join('---' for _ in headers) + '|'] +
                     ['| ' + ' | '.join(row) + ' |' for row in rows])


def build():
    qpath, cpath = ROOT / 'research/questions.org', ROOT / 'research/contrats.org'
    qtext, ctext = qpath.read_text(encoding='utf-8'), cpath.read_text(encoding='utf-8')
    contracts = read_contracts(parse_org(ctext))
    bank = read_questions(parse_org(qtext), contracts)
    bank['source'] = {'questions': 'research/questions.org', 'contracts': 'research/contrats.org',
                      'sha256': hashlib.sha256((qtext + '\0' + re.sub(r'#\+begin_generated.*?#\+end_generated', '', ctext, flags=re.S)).encode()).hexdigest()}
    for code in contracts:
        rows = []
        for q in bank['questions']:
            for p in q['productions']:
                if code in p['research']['targets'] or p['research']['contracts'].get(code) is not None:
                    value = p['research']['contracts'].get(code)
                    state = next((k for k, v in STATES.items() if v == value and type(v) is type(value)), '—')
                    rows.append([f'[[file:questions.org::#{p["id"]}][{p["id"]}]]', state,
                                 'cible' if code in p['research']['targets'] else 'comparaison'])
        block = f'#+begin_generated references-{code}\n' + (org_table(['Rédaction', 'Respect', 'Rôle'], rows) if rows else 'Aucune rédaction actuellement ; code conservé pour une extension du corpus.') + '\n#+end_generated'
        pattern = r'#\+begin_generated references-' + code + r'\n.*?\n#\+end_generated'
        require(len(re.findall(pattern, ctext, re.S)) == 1, f'{code}: bloc de renvois générés absent ou dupliqué')
        ctext = re.sub(pattern, lambda _: block, ctext, flags=re.S)
    public = {'version': bank['version'], 'questions': [
        {k: q[k] for k in ('id', 'level', 'domain', 'statement', 'family')} |
        {'productions': [{k: p[k] for k in ('id', 'content')} for p in q['productions']]}
        for q in bank['questions']]}
    encoded = lambda obj: json.dumps(obj, ensure_ascii=False, indent=2) + '\n'
    outputs = {cpath: ctext, ROOT / 'research/bank.json': encoded(bank),
               ROOT / 'research/codebook.json': encoded({c: v['label'] for c, v in contracts.items()}),
               ROOT / 'docs/site/data/bank.json': encoded(public)}
    ppath = ROOT / 'research/protocole.org'
    ptext = ppath.read_text(encoding='utf-8')
    quantities = [len(q['productions']) for q in bank['questions']]
    summary = (f'Banque {bank["version"]} : {len(quantities)} questions, {sum(quantities)} rédactions, '
               f'de {min(quantities)} à {max(quantities)} par question ; '
               f'{sum(quantities) * 4} valeurs finales au maximum (note et trois axes).')
    marker = r'#\+begin_generated effectifs\n.*?\n#\+end_generated'
    require(len(re.findall(marker, ptext, re.S)) == 1, 'Bloc effectifs absent du protocole')
    outputs[ppath] = re.sub(marker, lambda _: '#+begin_generated effectifs\n' + summary + '\n#+end_generated', ptext, flags=re.S)
    pairs = [[f'[[file:questions.org::#{p["research"]["indentationOf"]}][{p["research"]["indentationOf"]}]]',
              f'[[file:questions.org::#{p["id"]}][{p["id"]}]]', q['title']]
             for q in bank['questions'] for p in q['productions'] if 'indentationOf' in p['research']]
    pair_marker = r'#\+begin_generated indentations\n.*?\n#\+end_generated'
    require(len(re.findall(pair_marker, ptext, re.S)) == 1, 'Bloc des paires absent du protocole')
    pair_text = org_table(['Sans retrait', 'Avec retraits', 'Question'], pairs) if pairs else 'Aucune paire d’indentation.'
    outputs[ppath] = re.sub(pair_marker, lambda _: '#+begin_generated indentations\n' + pair_text + '\n#+end_generated', outputs[ppath], flags=re.S)
    virtual = outputs | {qpath: qtext}
    for path, text in virtual.items():
        if path.suffix == '.org':
            ids = re.findall(r'^\s*:CUSTOM_ID:\s+(\S+)\s*$', text, re.M)
            require(len(ids) == len(set(ids)), f'{path.name}: CUSTOM_ID dupliqué')
            check_links(path, text, virtual)
    return outputs, bank


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true', help='vérifier sans réécrire')
    args = parser.parse_args()
    try:
        outputs, bank = build()
        stale = [p for p, content in outputs.items() if not p.exists() or p.read_text(encoding='utf-8') != content]
        if args.check:
            require(not stale, 'Fichiers désynchronisés : ' + ', '.join(str(p.relative_to(ROOT)) for p in stale) + '\nExécuter npm --prefix docs run build:data puis enregistrer les sorties.')
        else:
            for path in stale:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(outputs[path], encoding='utf-8')
        print(f'Org {bank["version"]} : {len(bank["questions"])} questions ; '
              f'{sum(len(q["productions"]) for q in bank["questions"])} rédactions ; '
              + ('synchronisation vérifiée.' if args.check else 'données générées.'))
    except (ValueError, OSError, subprocess.CalledProcessError) as error:
        parser.exit(1, str(error) + '\n')


if __name__ == '__main__':
    main()
