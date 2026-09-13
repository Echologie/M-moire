"""Regression checks for the editorial contract, including rejection of drift."""
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('org_bank', ROOT / 'docs/scripts/build-bank.py')
bank = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = bank
spec.loader.exec_module(bank)


class OrgBankTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.questions = (ROOT / 'research/questions.org').read_text()
        cls.contracts = bank.read_contracts(bank.parse_org((ROOT / 'research/contrats.org').read_text()))

    def parse(self, text=None):
        return bank.read_questions(bank.parse_org(text or self.questions), self.contracts)

    def test_checkbox_false_unknown_and_not_applicable_are_distinct(self):
        parsed = self.parse()
        productions = {p['id']: p for q in parsed['questions'] for p in q['productions']}
        self.assertIs(productions['R09-7']['research']['contracts']['NAME'], False)
        self.assertIs(productions['R09-7']['research']['contracts']['SCOPE'], True)
        self.assertIsNone(productions['R09-4']['research']['contracts']['SCOPE'])
        self.assertEqual(productions['R17-5']['research']['contracts']['SCOPE'], 'discussable')

    def test_table_cell_is_authoritative_not_target_code(self):
        text = self.questions.replace('| [[#R01-3][R01-3]] | [ ] |', '| [[#R01-3][R01-3]] | [X] |')
        p = self.parse(text)['questions'][0]['productions'][2]
        self.assertIn('EQ', p['research']['targets'])
        self.assertIs(p['research']['contracts']['EQ'], True)

    def test_unknown_and_duplicate_properties_fail(self):
        for replacement in [':NIVEUA: 5e', ':NIVEAU: 5e\n:NIVEAU: 4e']:
            with self.subTest(replacement=replacement), self.assertRaises(ValueError):
                self.parse(self.questions.replace(':NIVEAU: 5e', replacement, 1))

    def test_missing_or_duplicated_coding_row_fails(self):
        row = '| [[#R01-1][R01-1]] | [X] | [X] |\n'
        self.assertIn(row, self.questions)
        for replacement in ['', row + row]:
            with self.subTest(replacement=replacement), self.assertRaises(ValueError):
                self.parse(self.questions.replace(row, replacement, 1))

    def test_unknown_contract_and_invalid_checkbox_fail(self):
        for text in [self.questions.replace('contrats.org::#EQ][EQ]', 'contrats.org::#UNKNOWN][UNKNOWN]', 1),
                     self.questions.replace('| [X] |', '| oui |', 1)]:
            with self.assertRaises(ValueError):
                self.parse(text)

    def test_unclosed_block_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'non fermé'):
            bank.parse_org('* Texte\n#+begin_verse\n  Ligne sans fin de bloc')

    def test_indentation_and_math_are_lossless(self):
        text = '* Texte\n#+begin_verse\n    $|x| < \\varepsilon$\n\n        Deuxième portée.\n#+end_verse'
        node = bank.parse_org(text).children[0]
        self.assertEqual(bank.verse(node), '    $|x| < \\varepsilon$\n\n        Deuxième portée.')
        parsed = self.parse()
        p = next(p for q in parsed['questions'] for p in q['productions'] if p['id'] == 'R17-7')
        self.assertIn('\n        On a', p['content'])
        self.assertIn('\n    Ainsi,', p['content'])
        self.assertIn('\nPour tout', p['content'])

    def test_indentation_pair_cannot_change_words(self):
        text = self.questions.replace('        On a $|u_n-2|', '        On suppose $|u_n-2|', 1)
        self.assertNotEqual(text, self.questions)
        with self.assertRaisesRegex(ValueError, 'uniquement par les retraits'):
            self.parse(text)

    def test_missing_anchor_and_impossible_pdf_page_fail(self):
        for link in ['[[file:contrats.org::#MISSING][MISSING]]',
                     '[[docview:../bib/Duval_1993_Argumenter_demontrer_expliquer.pdf::999][page 999]]']:
            with self.subTest(link=link), self.assertRaises(ValueError):
                bank.check_links(ROOT / 'research/questions.org', link, {})

    def test_cli_migrates_org_changes_and_rejects_manual_json_edits(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for sub in ['research', 'docs/scripts']:
                (root / sub).mkdir(parents=True)
            for file in ['questions.org', 'contrats.org', 'protocole.org']:
                shutil.copyfile(ROOT / 'research' / file, root / 'research' / file)
            shutil.copytree(ROOT / 'bib', root / 'bib')
            script = root / 'docs/scripts/build-bank.py'
            shutil.copyfile(ROOT / 'docs/scripts/build-bank.py', script)
            question_file = root / 'research/questions.org'
            text = question_file.read_text().replace('Dans un triangle $ABC$', 'On considère un triangle $ABC$', 1)
            question_file.write_text(text)
            command = [sys.executable, str(script)]
            generated = subprocess.run(command, capture_output=True, text=True)
            self.assertEqual(generated.returncode, 0, generated.stderr)
            public_file = root / 'docs/site/data/bank.json'
            public = json.loads(public_file.read_text())
            self.assertTrue(public['questions'][0]['statement'].startswith('On considère'))
            self.assertEqual(question_file.read_text(), text, 'La génération ne réécrit pas la source des questions')
            check = subprocess.run(command + ['--check'], capture_output=True, text=True)
            self.assertEqual(check.returncode, 0, check.stderr)
            public['questions'][0]['statement'] = 'Modification du seul JSON'
            public_file.write_text(json.dumps(public))
            edited = public_file.read_bytes()
            rejected = subprocess.run(command + ['--check'], capture_output=True, text=True)
            self.assertNotEqual(rejected.returncode, 0)
            self.assertIn('désynchronisés', rejected.stderr)
            self.assertEqual(public_file.read_bytes(), edited, '--check doit rester en lecture seule')


if __name__ == '__main__':
    unittest.main()
