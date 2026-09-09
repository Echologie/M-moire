'use strict';
const fs=require('fs'),vm=require('vm'),assert=require('node:assert/strict');
const noop=()=>{};const element={addEventListener:noop};const context={document:{querySelector:()=>element},window:{addEventListener:noop},fetch:()=>new Promise(()=>{}),setTimeout,console,assert,bankInput:JSON.parse(fs.readFileSync('docs/data/bank.json'))};vm.createContext(context);vm.runInContext(fs.readFileSync('docs/enquete.js','utf8'),context);
vm.runInContext(`
render=()=>{};showHelp=()=>{};bank=bankInput;
for(let repeat=0;repeat<100;repeat++){
 selectedLevels=[...LEVELS];seen=new Set();start();assert.equal(session.length,20);assert.equal(new Set(session.map(q=>q.family)).size,20);
 for(const q of session){assert.equal(orders[q.id].length,4);assert.equal(new Set(orders[q.id]).size,4);}
}
selectedLevels=['5e'];seen=new Set();start();assert.equal(session.length,2);assert.ok(session.every(q=>q.level==='5e'));
selectedLevels=['Sup 1'];start();assert.equal(session.length,12);assert.ok(session.every(q=>q.level==='Sup 1'));
const q=current();assert.equal(complete(q),false);for(const p of q.productions)for(const a of ['note',...required])response(p).scores[a]=0;assert.equal(complete(q),true);
const p=q.productions[0];delete response(p).scores.note;assert.equal(complete(q),false);response(p).scores.note='NA';assert.equal(complete(q),true);
const before=response(p).scores.lisibilite;response(p).scores.precision=3;axisX='lisibilite';axisY='justesse';assert.equal(response(p).scores.precision,3);assert.equal(response(p).scores.lisibilite,before);
const oldOrder=[...orders[q.id]];index=1;index=0;assert.deepEqual(orders[q.id],oldOrder);
selectedLevels=[...LEVELS];seen=new Set(bank.questions.slice(0,35).map(q=>q.id));start();assert.equal(session.length,5);assert.ok(session.every(q=>!seen.has(q.id)));
console.log('Session sampling, family exclusion, stable order, zero / missing / NA, independent scores and unseen preference: OK');
`,context);
