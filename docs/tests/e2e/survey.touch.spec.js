const { test, expect } = require('@playwright/test');
const { thumb, center, settled, driver, drag, note, close, start } = require('./gestures.cjs');
test.use({ reducedMotion: 'no-preference' });
test.setTimeout(90000);
test('gestes tactiles Android : tutoriel, glissement, défilement et rédaction suivante', async ({ page }) => {
  page.on('pageerror', error => { throw error; });
  const d = await driver(page, true); await start(page, d);
  await note(page, d); await close(page, d);
  const first = thumb(page, 'x'); await drag(d, first, 30);
  await expect.poll(async () => Number(await first.getAttribute('data-value'))).toBeGreaterThan(0);
  const value = await first.getAttribute('data-value');
  const y = thumb(page, 'y'); await y.scrollIntoViewIfNeeded();
  const beforeScroll = await page.evaluate(() => scrollY), p = await center(y);
  await d.down(p); await d.move({ x: p.x + 2, y: p.y - 90 }); await d.up();
  await expect.poll(() => page.evaluate(() => scrollY)).toBeGreaterThan(beforeScroll);
  await expect(page.locator('reading-card')).toHaveCount(0);
  await expect(y).toHaveAttribute('data-value', '0');
  await expect(first).toHaveAttribute('data-value', value);
  await d.click(page.locator('#next-production'));
  await expect(page.locator('#reader-title')).toHaveText('Rédaction 2');
  await note(page, d); await close(page, d);
  await d.click(thumb(page, 'y', 1)); await expect(page.locator('#reader-title')).toHaveText('Rédaction 1');
});

test('un geste tactile annulé ne valide ni la note ni la coordonnée', async ({ page }) => {
  const d = await driver(page, true); await start(page, d);
  const knob = page.locator('.grade-knob'), p = await center(knob);
  await d.down(p); await d.move({ x: p.x + 30, y: p.y }); await d.cancel();
  await expect(page.locator('#grade')).toHaveValue('1.5');
  await expect(page.locator('#grade')).toHaveClass(/ungraded/);
  await expect(page.locator('#place-button')).toBeDisabled();
  await note(page, d); await close(page, d);
  const x = thumb(page, 'x'); await x.scrollIntoViewIfNeeded(); const q = await center(x);
  await d.down(q); await d.move({ x: q.x + 30, y: q.y }); await d.cancel();
  await expect(x).toHaveAttribute('data-value', '0');
  await expect(x).toHaveClass(/pending/);
  await expect(page.locator('reading-card')).toHaveCount(0);
});
