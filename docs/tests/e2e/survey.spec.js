const { test, expect } = require('@playwright/test');
const fs = require('node:fs');

test.use({ reducedMotion: 'reduce' });
const plane = (page, id) => page.locator(`#view-${id}`);
async function tourStep(page, number) {
  await expect(page.locator('spotlight-guide')).toHaveAttribute('step', String(number));
}
async function rotate(page) {
  const box = await page.locator('#space').boundingBox();
  await page.mouse.move(box.x + box.width - 28, box.y + 65);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width - 115, box.y + 105, { steps: 8 });
  await page.mouse.up();
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
  await page.getByRole('button', { name: 'Prendre la main', exact: true }).click();
  await tourStep(page, 0);
  await expect(page.locator('#place-button')).toBeDisabled();
  await grade(page, 7);
  await tourStep(page, 1);
  await close(page);
  await tourStep(page, 2);
  await plane(page, 'xy').click();
  await tourStep(page, 3);
  await page.locator('.orb').press('Shift+ArrowRight');
  await tourStep(page, 4);
  await plane(page, 'xz').click();
  await tourStep(page, 5);
  await page.locator('.orb').press('Shift+ArrowUp');
  await tourStep(page, 6);
  await plane(page, '3d').click();
  await tourStep(page, 7);
  await rotate(page);
  await tourStep(page, 8);
  await page.locator('.orb').press('Enter');
  await tourStep(page, 9);
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
    if (await page.locator('reading-card').count()) {
      await grade(page, 6);
      await close(page);
    }
    await page.getByRole('button', { name: 'Passer cette question', exact: true }).click();
    await expect(page.locator('reading-card, .finish-panel')).toBeVisible();
  }
}

test('note indépendante, trois plans, révision et export traçable', async ({ page }) => {
  await start(page);
  await expect(page.locator('reading-card input')).toHaveCount(1);
  await expect(page.locator('#grade')).toHaveAttribute('step', '0.25');
  await expect(page.locator('main')).not.toContainText(/contrat|rédaction de référence/i);
  const firstId = await page.locator('reading-card').getAttribute('production-id');
  await grade(page, 9);
  await close(page);
  const first = page.locator(`.orb[data-id="${firstId}"]`);
  await plane(page, 'xy').click();
  await first.press('Shift+ArrowRight');
  await first.press('Shift+ArrowRight');
  await first.press('Shift+ArrowUp');
  await expect.poll(() => coordinates(first)).toEqual([2, 1, 0]);
  await plane(page, 'xz').click();
  await first.press('Shift+ArrowUp');
  await first.press('Shift+ArrowUp');
  await first.press('Shift+ArrowUp');
  await expect.poll(() => coordinates(first)).toEqual([2, 1, 3]);
  await plane(page, 'yz').click();
  await first.press('Shift+ArrowRight');
  await expect.poll(() => coordinates(first)).toEqual([2, 2, 3]);
  await plane(page, '3d').click();
  await rotate(page);
  await expect.poll(() => coordinates(first)).toEqual([2, 2, 3]);
  await page.locator('#next-production').click();
  await expect(page.locator('#reader-title')).toHaveText('Rédaction 2');
  await expect(page.locator('.orb')).toHaveCount(2);
  await grade(page, 0);
  await close(page);
  await page.getByRole('button', { name: 'Relire la rédaction 1', exact: true }).click();
  await expect(page.locator('#grade')).toHaveValue('2.25');
  await grade(page, 6);
  await close(page);
  await page.getByRole('button', { name: 'Comparer', exact: true }).click();
  await expect(page.locator('.comparison-columns rich-text')).toHaveCount(2);
  await page.getByRole('button', { name: 'Fermer la comparaison', exact: true }).click();
  await finishBySkipping(page);
  const downloadEvent = page.waitForEvent('download');
  await page.getByRole('button', { name: 'Exporter mes réponses', exact: true }).click();
  const exported = JSON.parse(fs.readFileSync(await (await downloadEvent).path(), 'utf8'));
  expect(exported.schemaVersion).toBe(2);
  expect(exported.randomization.algorithm).toBe('mulberry32-fisher-yates-v1');
  expect(Number.isInteger(exported.randomization.seed)).toBe(true);
  expect(exported.questionOrder).toHaveLength(2);
  expect(exported.answers[firstId]).toMatchObject({ note: 1.5, initialNote: 2.25, coordinates: { x: 2, y: 2, z: 3 } });
  expect(exported.answers[firstId].evaluatedAxes.sort()).toEqual(['x', 'y', 'z']);
  expect(Object.values(exported.answers).some(a => a.note === 0)).toBe(true);
  expect(exported.answers['practice-1']).toBeUndefined();
  expect(exported.events.every(e => e.questionId !== 'practice')).toBe(true);
  expect(exported.events.some(e => e.event === 'orbit')).toBe(true);
  expect(exported.gradeScale).toEqual({ min: 0, max: 3, step: 0.25 });
});

test('origine explicite, déplacement à la souris et retour à une question', async ({ page }) => {
  await start(page, '5e');
  await grade(page, 6);
  await close(page);
  const firstId = await page.locator('.orb').getAttribute('data-id');
  await plane(page, 'xy').click();
  await page.locator('#confirm-position').click();
  await plane(page, 'xz').click();
  await page.locator('#confirm-position').click();
  await expect(page.locator('.orb')).not.toHaveClass(/pending/);
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual([0, 0, 0]);
  const box = await page.locator('.orb').boundingBox();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 70, box.y + box.height / 2 - 40, { steps: 8 });
  await page.mouse.up();
  await expect.poll(async () => (await coordinates(page.locator('.orb')))[0]).toBeGreaterThan(0);
  const before = await coordinates(page.locator('.orb'));
  expect(before[1]).toBe(0);
  await page.getByRole('button', { name: 'Passer cette question', exact: true }).click();
  await grade(page, 5);
  await close(page);
  await page.getByRole('button', { name: '← Question précédente', exact: true }).click();
  await expect(page.locator('reading-card')).toHaveAttribute('production-id', firstId);
  await expect(page.locator('#grade')).toHaveValue('1.5');
  await close(page);
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual(before);
});

test('tutoriel et lecture sur téléphone, puis rotation de l’écran', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await start(page, '5e');
  const question = await page.locator('#question-panel').boundingBox();
  await expect(page.locator('#question-panel')).toBeInViewport({ ratio: 1 });
  const reader = await page.locator('reading-card').boundingBox();
  expect(reader.width).toBeGreaterThan(350);
  expect(reader.y).toBeGreaterThanOrEqual(question.y + question.height);
  expect(reader.height).toBeGreaterThan(500);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  await grade(page, 8);
  await close(page);
  await plane(page, 'yz').click();
  await page.locator('.orb').press('Shift+ArrowUp');
  await page.setViewportSize({ width: 844, height: 390 });
  await expect.poll(() => coordinates(page.locator('.orb'))).toEqual([0, 0, 1]);
  await page.locator('.orb').press('Enter');
  await expect(page.locator('#grade')).toHaveValue('2');
  await expect(page.locator('#place-button')).toBeInViewport();
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
});
