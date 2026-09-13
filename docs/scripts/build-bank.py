"""Publish a strict allowlist from the research bank; never expose annotations."""
import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
bank = json.loads((ROOT / 'research/bank.json').read_text())
public = {'version': bank['version'], 'questions': []}
for q in bank['questions']:
    # Family IDs are opaque; contract vocabulary is never included in the payload.
    public['questions'].append({k: q[k] for k in ('id', 'level', 'domain', 'statement', 'family')} | {
        'productions': [{k: p[k] for k in ('id', 'content')} for p in q['productions']]
    })
destination = ROOT / 'docs/site/data/bank.json'
destination.parent.mkdir(parents=True, exist_ok=True)
destination.write_text(json.dumps(public, ensure_ascii=False, indent=2) + '\n')
lines = ['* Notices de conception — banque ' + bank['version'], '',
         'Document interne de conception. Les codes indiquent des cibles ou des contrôles, jamais une note attendue. Lire chaque analyse.', '']
for q in bank['questions']:
    lines += [f"** {q['id']} — {q['level']} — {q['title']}", '', q['statement'], '', 'Réponse de référence : ' + q['referenceAnswer'], '']
    for p in q['productions']:
        lines += [f"*** {p['id']}", '', p['content'], '', 'Cibles / contrôles : ' + ', '.join(p['research']['targets']), '', p['research']['analysis'], '']
(ROOT / 'research/notices.org').write_text('\n'.join(lines))
print(f"{len(bank['questions'])} questions ; {sum(len(q['productions']) for q in bank['questions'])} rédactions")
