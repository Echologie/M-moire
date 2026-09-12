const { test, expect } = require('@playwright/test');
const fs = require('node:fs');

test.use({ reducedMotion: 'reduce' });
const axis = (page, name) => page.locator(`#axis-${name}`);
const thumb = (page, name, number = 1) => axis(page, name).getByRole('slider', { name: `Rédaction ${number} ·`, exact: false });
async function tourStep(page, number) {
  await expect(page.locator('spotlight-guide')).toHaveAttribute('step', String(number));
}
async function rotate(page) {
  await page.locator('#space').press('ArrowRight');
}
async function grade(page, quarters) {
  const slider = page.getByRole('slider', { name: 'Note sur 3' });
  await slider.press('Home');
  for (let i = 0; i < quarters; i++) await slider.press('ArrowRight');
  await expect(slider).toHaveValue(String(quarters / 4));
}
async function close(page) {
  await page.locator('#place-button').click();
  await expect(page.locator('reading-card')).toHaveCount(0);
}
async function start(page, level = '3e') {
  await page.goto('/');
  await page.getByRole('checkbox', { name: level, exact: true }).check();
  await page.getByRole('button', { name: 'Commencer', exact: true }).click();
  await tourStep(page, 0);
  await expect(page.locator('#place-button')).toBeDisabled();
  await grade(page, 7);
  await tourStep(page, 1);
  await close(page);
  await tourStep(page, 2);
  await thumb(page, 'x').press('Shift+ArrowRight');
  await tourStep(page, 3);
  await thumb(page, 'y').press('Shift+ArrowRight');
  await tourStep(page, 4);
  await thumb(page, 'z').press('Shift+ArrowRight');
  await tourStep(page, 5);
  await rotate(page);
  await tourStep(page, 6);
  await thumb(page, 'x').press('Enter');
  await tourStep(page, 7);
  await page.getByRole('button', { name: 'Commencer mes questions', exact: true }).click();
  await expect(page.locator('spotlight-guide')).toHaveCount(0);
  await expect(page.locator('reading-card')).toBeVisible();
  await expect(page.locator('.orb')).toHaveCount(1);
  await expect(page.locator('#grade')).toHaveClass(/ungraded/);
}
async function coordinates(orb) {
  return orb.evaluate(el => ['x', 'y', 'z'].map(a => Number(el.dataset[a])));
}
async function finishBySkipping(page) {
  while (await page.getByRole('button', { name: 'Passer cette question', exact: true }).count()) {
    if (await page.locator('reading-card').count()) { await grade(page, 6); await close(page); }
    await page.getByRole('button', { name: 'Passer cette question', exact: true }).click();
    await expect(page.locator('reading-card, .finish-panel')).toBeVisible();
  }
}

test('trois curseurs indépendants, note révisable et export traçable', async ({ page }) => {
  await start(page);
  await expect(page.locator('reading-card input')).toHaveCount(1);
  await expect(page.locator('#grade')).toHaveAttribute('step', '0.25');
  await expect(page.locator('main')).not.toContainText(/contrat|rédaction de référence|−10|\+10/i);
  const firstId = await page.locator('reading-card').getAttribute('production-id');
  await grade(page, 9); await close(page);
  const first = page.locator(`.orb[data-id="${firstId}"]`);
  for (let i = 0; i < 2; i++) await thumb(page, 'x').press('Shift+ArrowRight');
  await expect.poll(() => coordinates(first)).toEqual([2, 0, 0]);
  for (let i = 0; i < 2; i++) await thumb(page, 'y').press('Shift+ArrowRight');
  await expect.poll(() => coordinates(first)).toEqual([2, 2, 0]);
  for (let i = 0; i < 3; i++) await thumb(page, 'z').press('Shift+ArrowRight');
  await expect.poll(() => coordinates(first)).toEqual([2, 2, 3]);
  await rotate(page);
  await expect.poll(() => coordinates(first)).toEqual([2, 2, 3]);
  await page.locator('#next-production').click();
  await expect(page.locator('#reader-title')).toHaveText('Rédaction 2');
  await expect(page.locator('.orb')).toHaveCount(2);
  await grade(page, 0); await close(page);
  await thumb(page, 'x', 1).click();
  await expect(page.locator('#grade')).toHaveValue('2.25');
  await grade(page, 6); await close(page);
  await page.getByRole('button', { name: 'Comparer', exact: true }).click();
  await expect(page.locator('.comparison-columns rich-text')).toHaveCount(2);
  await page.getByRole('button', { name: 'Fermer la comparaison', exact: true }).click();
  await finishBySkipping(page);
  const downloadEvent = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Exporter mes réponses', exact: true }).click();
  const exported = JSON.parse(fs.readFileSync(await (await downloadEvent).path(), 'utf8'));
  expect(exported.schemaVersion).toBe(2);
  expect(exported.interactionMode).toBe('axis-sliders');
  expect(exported.randomization.algorithm).toBe('mulberry32-fisher-yates-v1');
  expect(Number.isInteger(exported.randomization.seed)).toBe(true);
  expect(exported.questionOrder).toHaveLength(2);
  expect(exported.answers[firstId]).toMatchObject({ note: 1.5, initialNote: 2.25, coordinates: { x: 2, y: 2, z: 3 } });
  expect(exported.answers[firstId].evaluatedAxes.sort()).toEqual(['x', 'y', 'z']);
  expect(Object.values(exported.answers).some(a => a.note === 0)).toBe(true);
  expect(exported.answers['practice-1']).toBeUndefined();
  expect(exported.events.every(e => e.questionId !== 'practice')).toBe(true);
  expect(exported.events.some(e => e.event === 'orbit')).toBe(true);
  expect(exported.events.filter(e => e.event === 'place').every(e => ['x','y','z'].includes(e.axis))).toBe(true);
  expect(exported.gradeScale).toEqual({ min: 0, max: 3, step: 0.25 });
});

test('la saisie ne saute pas, le centre se confirme et la caméra garde les coordonnées', async ({ page }) => {
  await start(page, '5e');
  await grade(page, 6);
  const knob = await page.locator('.grade-knob').boundingBox();
  await page.mouse.move(knob.x + knob.width / 2 + 45, knob.y + knob.height / 2);
  await page.mouse.down();
  await expect(page.locator('#grade')).toHaveValue('1.5');
  await page.mouse.move(knob.x + knob.width / 2 + 75, knob.y + knob.height / 2, { steps: 6 });
  await page.mouse.up();
  const note = Number(await page.locator('#grade').inputValue());
  expect(note).toBeGreaterThan(1.5); expect(note).toBeLessThan(2.5);
  await close(page);
  await page.locator('#confirm-position').click();
  await expect(page.locator('.orb')).not.toHaveClass(/pending/);
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual([0, 0, 0]);
  const firstId = await page.locator('.orb').getAttribute('data-id');
  const box = await thumb(page, 'x').boundingBox();
  await page.mouse.move(box.x + box.width / 2 + 17, box.y + box.height / 2);
  await page.mouse.down();
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual([0, 0, 0]);
  await page.mouse.move(box.x + box.width / 2 + 67, box.y + box.height / 2, { steps: 8 });
  await expect.poll(async () => (await coordinates(page.locator('.orb')))[0]).toBeGreaterThan(0);
  await page.mouse.up();
  const before = await coordinates(page.locator('.orb'));
  expect(before[1]).toBe(0); expect(before[2]).toBe(0);
  const space = await page.locator('#space').boundingBox();
  await page.mouse.move(space.x + space.width - 35, space.y + 60);
  await page.mouse.down(); await page.mouse.move(space.x + space.width - 110, space.y + 100, { steps: 8 }); await page.mouse.up();
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual(before);
  await page.getByRole('button', { name: 'Passer cette question', exact: true }).click();
  await grade(page, 5); await close(page);
  await page.getByRole('button', { name: '← Question précédente', exact: true }).click();
  await expect(page.locator('reading-card')).toHaveAttribute('production-id', firstId);
  await expect(page.locator('#grade')).toHaveValue(String(note));
  await close(page);
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual(before);
});

test('billes proches dégagées, cliquables et toujours à leur vraie coordonnée', async ({ page }) => {
  await start(page);
  await grade(page, 6); await close(page); await page.locator('#confirm-position').click();
  await page.locator('#next-production').click();
  await grade(page, 7); await close(page);
  const first = thumb(page, 'x', 1), second = thumb(page, 'x', 2);
  await expect(first).toHaveAttribute('data-lift', '42');
  await expect(second).toHaveAttribute('data-lift', '0');
  await expect(first).toHaveAttribute('data-value', '0');
  await expect(second).toHaveAttribute('data-value', '0');
  await second.press('ArrowRight');
  await expect(first).toHaveAttribute('data-lift', '42');
  await expect(second).toHaveAttribute('data-value', '0.1');
  await expect(first).toHaveAttribute('data-value', '0');
  await page.screenshot({ path: test.info().outputPath('billes-proches.png'), fullPage: true });
  await first.click(); await expect(page.locator('#reader-title')).toHaveText('Rédaction 1'); await close(page);
  await second.click(); await expect(page.locator('#reader-title')).toHaveText('Rédaction 2'); await close(page);
  await second.press('End');
  await expect(first).toHaveAttribute('data-lift', '0');
  await expect(second).toHaveAttribute('data-value', '10');
  await expect(first).toHaveAttribute('data-value', '0');
  await expect(page.locator('#space .axis-label small, #space svg text')).toHaveCount(0);
});

test('curseurs au-dessus sur téléphone et lecture après rotation de l’écran', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await start(page, '5e');
  const question = await page.locator('#question-panel').boundingBox();
  await expect(page.locator('#question-panel')).toBeInViewport({ ratio: 1 });
  const reader = await page.locator('reading-card').boundingBox();
  expect(reader.width).toBeGreaterThan(350);
  expect(reader.y).toBeGreaterThanOrEqual(question.y + question.height);
  expect(reader.height).toBeGreaterThan(500);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  await page.screenshot({ path: test.info().outputPath('note-mobile.png'), fullPage: true });
  await grade(page, 8); await close(page);
  const bars = await page.locator('#axes-panel').boundingBox(), scene = await page.locator('#space').boundingBox();
  expect(bars.y + bars.height).toBeLessThanOrEqual(scene.y);
  expect(bars.height).toBeLessThan(350);
  expect(scene.y).toBeLessThan(570);
  await thumb(page, 'z').press('Shift+ArrowRight');
  await page.screenshot({ path: test.info().outputPath('axes-mobile.png'), fullPage: true });
  await page.setViewportSize({ width: 844, height: 390 });
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual([0, 0, 1]);
  await page.locator('.orb').press('Enter');
  await expect(page.locator('#grade')).toHaveValue('2');
  await expect(page.locator('#place-button')).toBeInViewport();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
