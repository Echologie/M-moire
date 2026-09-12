const { test, expect } = require('@playwright/test');
const fs = require('node:fs');
const { thumb, center, settled, driver, drag, note, close, start } = require('./gestures.cjs');

test.use({ reducedMotion: 'no-preference' });
test.setTimeout(90000);
test.beforeEach(async ({ page }) => {
  page.on('pageerror', error => { throw error; });
});

test('parcours entier à la souris : suivant valide aussi les coordonnées conservées', async ({ page }) => {
  const d = await driver(page); await start(page, d);
  const answers = new Map();
  while (await page.locator('reading-card').count()) {
    const id = await page.locator('reading-card').getAttribute('production-id');
    await note(page, d); const value = Number(await page.locator('#grade').inputValue());
    await close(page, d);
    // The participant can intentionally keep two or all three axes at the centre.
    if (answers.size % 2 === 0) await drag(d, page.locator('#axis-x .slider-thumb.selected'), 25);
    const coordinates = await page.locator(`.orb[data-id="${id}"]`).evaluate(el => Object.fromEntries(['x','y','z'].map(a => [a, Number(el.dataset[a])])));
    answers.set(id, { note: value, coordinates });
    await d.click(page.locator('#next-production'));
    await expect(page.locator('reading-card, .finish-panel')).toBeVisible();
    if (await page.locator('reading-card').count()) await settled(page.locator('reading-card'));
  }
  expect(answers.size).toBeGreaterThan(3);
  const download = page.waitForEvent('download');
  await d.click(page.getByRole('button', { name: 'Exporter mes réponses', exact: true }));
  const result = JSON.parse(fs.readFileSync(await (await download).path(), 'utf8'));
  expect(result.skippedQuestions).toEqual([]);
  expect(Object.keys(result.answers)).toHaveLength(answers.size);
  for (const [id, answer] of answers) {
    expect(result.answers[id]).toMatchObject(answer);
    expect(result.answers[id].evaluatedAxes.sort()).toEqual(['x','y','z']);
  }
});

test('une bille surélevée reste sous la souris pendant toute la saisie', async ({ page }) => {
  const d = await driver(page); await start(page, d);
  await note(page, d); await close(page, d); await d.click(page.locator('#confirm-position'));
  await d.click(page.locator('#next-production')); await note(page, d); await close(page, d);
  const first = thumb(page, 'x', 1), second = thumb(page, 'x', 2);
  await settled(first); const from = await center(first);
  await expect(first).toHaveAttribute('data-lift', '42');
  await d.down(from);
  await expect(first).toHaveAttribute('data-value', '0');
  await d.move({ x: from.x + 18, y: from.y });
  await expect.poll(async () => Math.abs((await center(first)).y - from.y)).toBeLessThan(1);
  await expect.poll(async () => Number(await first.getAttribute('data-value'))).toBeGreaterThan(0);
  await expect(second).toHaveAttribute('data-value', '0');
  await d.move({ x: from.x + 40, y: from.y });
  await expect.poll(async () => Math.abs((await center(first)).y - from.y)).toBeLessThan(1);
  await d.up(); await settled(first);
  await expect(page.locator('reading-card')).toHaveCount(0);
  await d.click(second); await expect(page.locator('#reader-title')).toHaveText('Rédaction 2');
  await close(page, d); await d.click(first); await expect(page.locator('#reader-title')).toHaveText('Rédaction 1');
});

test('la bille de note reflète la nouvelle fiche et ne saute pas lors d’une saisie imprécise', async ({ page }) => {
  const d = await driver(page); await start(page, d);
  const knob = page.locator('.grade-knob'), input = page.locator('#grade'), rail = page.locator('.grade-rail');
  const k = await center(knob), r = await rail.boundingBox();
  expect(Math.abs(k.x - (r.x + Number(await input.inputValue()) / 3 * r.width))).toBeLessThan(1);
  await d.down({ x: k.x + 20, y: k.y });
  await expect(input).toHaveValue('1.5');
  await expect.poll(async () => (await center(knob)).x).toBeCloseTo(k.x, 0);
  await d.move({ x: k.x + 60, y: k.y }); await d.up();
  await expect(input).not.toHaveClass(/ungraded/);
  const n = Number(await input.inputValue());
  expect(n).toBeGreaterThan(1.5);
  expect(n * 4).toBe(Math.round(n * 4));
  await close(page, d); await d.click(page.locator('#confirm-position'));
  await d.click(page.locator('#next-production')); await settled(page.locator('reading-card'));
  await expect(input).toHaveValue('1.5');
  const next = await center(knob), nextRail = await rail.boundingBox();
  expect(Math.abs(next.x - nextRail.x - nextRail.width / 2)).toBeLessThan(1);
});
