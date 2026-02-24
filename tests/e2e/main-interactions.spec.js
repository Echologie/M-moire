const { test, expect } = require('@playwright/test');

async function cardPressAndRelease(locator) {
  const box = await locator.boundingBox();
  if (!box) throw new Error('carte introuvable');
  const x = box.x + box.width / 2;
  const y = box.y + box.height / 2;
  await locator.dispatchEvent('mousedown', { button: 0, clientX: x, clientY: y });
  await locator.dispatchEvent('mouseup', { button: 0, clientX: x, clientY: y });
}

test('drag de carte conserve 4 cartes uniques', async ({ page }) => {
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

  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);
  await expect(cardA).toHaveAttribute('data-state', 'mini');
});

test('clic sur carte ouvre puis referme la meme carte', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);
  const cardB = page.getByTestId('card-B');
  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);

  await cardPressAndRelease(cardB);
  await expect(cardB).toHaveAttribute('data-state', 'expanded');
  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);

  await cardB.click({ position: { x: 20, y: 20 }, force: true });
  await expect(cardB).toHaveAttribute('data-state', 'mini');
  await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);
});

test.describe('mobile emulation', () => {
  test.use({
    viewport: { width: 393, height: 851 },
    hasTouch: true
  });

  test('mobile: ouverture/fermeture de la carte unique', async ({ page }) => {
    await page.goto('/index.html');
    await page.waitForTimeout(200);

    const cardC = page.getByTestId('card-C');
    await expect(cardC).toBeVisible();
    await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);

    await cardPressAndRelease(cardC);
    await expect(cardC).toHaveAttribute('data-state', 'expanded');

    await cardC.click({ position: { x: 24, y: 24 }, force: true });
    await expect(cardC).toHaveAttribute('data-state', 'mini');
    await expect(page.locator('[data-testid^="card-"]')).toHaveCount(4);
  });
});
