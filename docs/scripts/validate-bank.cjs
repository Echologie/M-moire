'use strict';
const fs = require('node:fs'), path = require('node:path'), assert = require('node:assert/strict');
const root = path.resolve(__dirname, '../..');
const read = p => fs.readFileSync(path.join(root, p), 'utf8');
const bank = JSON.parse(read('research/bank.json')), publicBank = JSON.parse(read('docs/site/data/bank.json')), codes = JSON.parse(read('research/codebook.json'));
const katex = require('../site/vendor/katex/katex.min.js');
assert.equal(publicBank.version, bank.version);
assert.equal(publicBank.questions.length, bank.questions.length);
const ids = new Set(), qids = new Set(), levels = {}, counts = {}, variants = {};
let formulas = 0;
for (const [i, q] of bank.questions.entries()) {
  assert.ok(!qids.has(q.id), 'duplicate question'); qids.add(q.id);
  assert.ok(q.productions.length >= 2, q.id);
  variants[q.productions.length] = (variants[q.productions.length] || 0) + 1;
  levels[q.level] = (levels[q.level] || 0) + 1;
  const pub = publicBank.questions[i];
  assert.deepEqual(Object.keys(pub).sort(), ['domain', 'family', 'id', 'level', 'productions', 'statement']);
  for (const field of ['id', 'level', 'domain', 'statement', 'family']) assert.equal(pub[field], q[field]);
  assert.equal(pub.productions.length, q.productions.length);
  for (const [j, p] of q.productions.entries()) {
    assert.ok(!ids.has(p.id), p.id); ids.add(p.id);
    assert.deepEqual(pub.productions[j], { id: p.id, content: p.content });
    assert.ok(p.research.analysis.length > 15);
    for (const code of p.research.targets) { assert.ok(codes[code], code); counts[code] = (counts[code] || 0) + 1; }
  }
  for (const s of [q.statement, q.referenceAnswer, ...q.productions.map(p => p.content)]) {
    assert.equal((s.match(/\$/g) || []).length % 2, 0, q.id);
    for (const m of s.matchAll(/\$([^$]+)\$/g)) {
      assert.ok(!m[1].includes('\n'), `${q.id}: unexpected newline inside inline mathematics`);
      katex.renderToString(m[1], { throwOnError: true, strict: 'ignore' }); formulas++;
    }
  }
}
assert.ok(Object.keys(variants).length > 1, 'Variant counts must reflect the question design');
assert.equal(read('docs/site/index.html'), read('docs/site/enquete.html'));
for (const file of ['index.html', 'enquete.html', 'prototype.html']) {
  for (const m of read('docs/site/' + file).matchAll(/(?:src|href)="([^"]+)"/g)) {
    if (!/^(https?:|data:|#)/.test(m[1])) {
      const asset = decodeURIComponent(new URL(m[1], 'https://local.invalid/').pathname).slice(1);
      assert.ok(fs.existsSync(path.join(root, 'docs/site', asset)), m[1]);
    }
  }
}
const report = { version: bank.version, questions: qids.size, productions: ids.size, variantsPerQuestion: variants, formulas, levels, targetOccurrences: counts, checks: ['Unique stable identifiers', 'Variable production counts', 'Strict public allowlist', 'Valid mathematical markup', 'Identical entrypoints', 'Local assets present'] };
fs.writeFileSync(path.join(root, 'research/audit.json'), JSON.stringify(report, null, 2) + '\n');
console.log(`${qids.size} questions, ${ids.size} productions, ${formulas} formules : validation réussie.`);
