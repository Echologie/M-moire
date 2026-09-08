const { test, expect } = require('@playwright/test');

async function centerOf(locator) {
  const box = await locator.boundingBox();
  if (!box) throw new Error('carte introuvable');
  return {
    x: box.x + box.width / 2,
    y: box.y + box.height / 2,
    width: box.width,
    height: box.height
  };
}

function expectCentersClose(a, b, tolerance = 1.5) {
  expect(Math.abs(a.x - b.x)).toBeLessThanOrEqual(tolerance);
  expect(Math.abs(a.y - b.y)).toBeLessThanOrEqual(tolerance);
}

test('drag de carte conserve 4 cartes uniques et ne declenche pas le zoom', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);

  const cardA = page.getByTestId('card-A');
  await expect(cardA).toBeVisible();
  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);

  const box = await cardA.boundingBox();
  if (!box) throw new Error('card-A introuvable');

  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 180, box.y + box.height / 2 + 120, { steps: 12 });
  await page.mouse.up();

  await page.waitForTimeout(160);
  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);
  await expect(cardA).toHaveAttribute('data-state', 'mini');
});

test('zoom: meme noeud DOM, centre invariant et fermeture par le plateau', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);

  const cardB = page.getByTestId('card-B');
  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);

  await page.evaluate(() => {
    window.__originalCardB = document.querySelector('[data-testid="card-B"]');
  });

  const before = await centerOf(cardB);
  await cardB.click();
  await expect(cardB).toHaveAttribute('data-state', 'expanded');

  await page.waitForTimeout(70);
  const duringOpen = await centerOf(cardB);
  expectCentersClose(before, duringOpen);

  await page.waitForTimeout(220);
  const opened = await centerOf(cardB);
  expectCentersClose(before, opened);
  expect(opened.width).toBeGreaterThan(before.width * 2.5);
  expect(opened.height).toBeGreaterThan(before.height * 2.5);

  expect(await page.evaluate(() =>
    window.__originalCardB === document.querySelector('[data-testid="card-B"]')
  )).toBe(true);

  await page.getByTestId('board').click({ position: { x: 8, y: 8 } });
  await expect(cardB).toHaveAttribute('data-state', 'mini');

  await page.waitForTimeout(70);
  const duringClose = await centerOf(cardB);
  expectCentersClose(before, duringClose);

  await page.waitForTimeout(220);
  const closed = await centerOf(cardB);
  expectCentersClose(before, closed);
  expect(Math.abs(closed.width - before.width)).toBeLessThanOrEqual(2);
  expect(Math.abs(closed.height - before.height)).toBeLessThanOrEqual(2);

  expect(await page.evaluate(() =>
    window.__originalCardB === document.querySelector('[data-testid="card-B"]')
  )).toBe(true);
  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);
});

test('une seule carte peut etre ouverte globalement', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);

  const cardA = page.getByTestId('card-A');
  const cardB = page.getByTestId('card-B');

  await cardA.click();
  await expect(cardA).toHaveAttribute('data-state', 'expanded');
  await expect(page.locator('[data-state="expanded"]')).toHaveCount(1);

  await cardB.click({ force: true });
  await expect(cardA).toHaveAttribute('data-state', 'mini');
  await expect(cardB).toHaveAttribute('data-state', 'expanded');
  await expect(page.locator('[data-state="expanded"]')).toHaveCount(1);
});

test.describe('mobile emulation', () => {
  test.use({
    viewport: { width: 393, height: 851 },
    hasTouch: true
  });

  test('mobile: tap ouvre la meme carte et tap sur le plateau la referme', async ({ page }) => {
    await page.goto('/index.html');
    await page.waitForTimeout(200);

    const cardC = page.getByTestId('card-C');
    await expect(cardC).toBeVisible();
    await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);

    await page.evaluate(() => {
      window.__originalCardC = document.querySelector('[data-testid="card-C"]');
    });

    await cardC.tap();
    await expect(cardC).toHaveAttribute('data-state', 'expanded');
    expect(await page.evaluate(() =>
      window.__originalCardC === document.querySelector('[data-testid="card-C"]')
    )).toBe(true);

    const board = await page.getByTestId('board').boundingBox();
    if (!board) throw new Error('plateau introuvable');
    await page.touchscreen.tap(board.x + 8, board.y + 8);

    await expect(cardC).toHaveAttribute('data-state', 'mini');
    await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);
  });
});
