'use strict';
const fs=require('fs'),vm=require('vm'),assert=require('node:assert/strict'),path=require('path');
const root=path.resolve(__dirname,'..');process.chdir(root);
const bank=JSON.parse(fs.readFileSync('research/bank.json')),publicBank=JSON.parse(fs.readFileSync('docs/data/bank.json')),codes=JSON.parse(fs.readFileSync('research/codebook.json'));
assert.equal(bank.questions.length,40);let ids=new Set(),formulas=0,counts={},levels={};const ctx={};vm.createContext(ctx);vm.runInContext(fs.readFileSync('docs/vendor/katex/katex.min.js','utf8'),ctx);
for(const [i,q] of bank.questions.entries()){
 levels[q.level]=(levels[q.level]||0)+1;assert.equal(q.productions.length,4);assert.equal(q.id,publicBank.questions[i].id);assert.ok(!('referenceAnswer' in publicBank.questions[i]));
 for(const [j,p] of q.productions.entries()){assert.ok(!ids.has(p.id));ids.add(p.id);assert.ok(!('research' in publicBank.questions[i].productions[j]));assert.equal(p.content,publicBank.questions[i].productions[j].content);for(const t of p.research.targets){assert.ok(codes[t],t);counts[t]=(counts[t]||0)+1;}}
 for(const s of [q.statement,...q.productions.map(p=>p.content)]){assert.equal((s.match(/\$/g)||[]).length%2,0);for(const m of s.matchAll(/\$([^$]+)\$/g)){ctx.katex.renderToString(m[1],{throwOnError:true,strict:'ignore'});formulas++;}}
}
for(const f of ['index.html','enquete.html','enquete.css','enquete.js','data/bank.json','prototype.html'])assert.deepEqual(fs.readFileSync('www/'+f),fs.readFileSync('docs/'+f),f);
for(const f of ['index.html','enquete.html'])for(const m of fs.readFileSync('docs/'+f,'utf8').matchAll(/(?:src|href)="([^"]+)"/g))assert.ok(fs.existsSync('docs/'+m[1]),m[1]);
const pair=bank.questions.filter(q=>q.family==='lineaire');assert.equal(pair.length,2);assert.equal(pair[0].statement,pair[1].statement);assert.deepEqual(pair[0].productions.map(p=>p.content),pair[1].productions.map(p=>p.content));
const report={questions:bank.questions.length,productions:ids.size,formulas,levels,targetOccurrences:counts,checks:['Unique stable IDs','Four variants per question','Public data without research annotations','Valid KaTeX syntax','Published assets synchronized','Local resource links','Identical cross-level pair']};fs.writeFileSync('research/audit.json',JSON.stringify(report,null,2)+'\n');console.log(JSON.stringify(report,null,2));
