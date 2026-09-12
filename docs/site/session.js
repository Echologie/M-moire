export const axes = {
  x: { negative: 'Confus', positive: 'Lisible', min: -10, max: 10, zero: 'Attendu' },
  y: { negative: 'Vague', positive: 'Précis', min: -10, max: 10, zero: 'Attendu' },
  z: { negative: 'Fautif', positive: 'Valide', min: -10, max: 10, zero: 'Attendu' }
};
export function createSeed() { return crypto.getRandomValues(new Uint32Array(1))[0]; }
export function randomFrom(seed) {
  let value = seed >>> 0;
  return () => { value += 0x6D2B79F5; let t = value; t = Math.imul(t ^ t >>> 15, t | 1); t ^= t + Math.imul(t ^ t >>> 7, t | 61); return ((t ^ t >>> 14) >>> 0) / 4294967296; };
}
export function shuffle(values, random) {
  const result = [...values];
  for (let i = result.length - 1; i > 0; i--) { const j = Math.floor(random() * (i + 1)); [result[i], result[j]] = [result[j], result[i]]; }
  return result;
}
export function prepareSession(bank, levels, seed, seen = new Set()) {
  const random = randomFrom(seed);
  const pool = bank.questions.filter(q => levels.includes(q.level));
  const fresh = pool.filter(q => !seen.has(q.id));
  const families = new Set();
  return shuffle(fresh.length ? fresh : pool, random).filter(q => {
    if (families.has(q.family)) return false; families.add(q.family); return true;
  }).slice(0, 20).map(q => ({ ...q, productions: shuffle(q.productions, random) }));
}
