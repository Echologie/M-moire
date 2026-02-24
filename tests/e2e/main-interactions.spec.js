const { test, expect } = require('@playwright/test');

test('drag ne doit pas ouvrir d overlay', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);

  const miniA = page.getByTestId('mini-A');
  await expect(miniA).toBeVisible();

  const box = await miniA.boundingBox();
  if (!box) throw new Error('mini-A introuvable');

  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 180, box.y + box.height / 2 + 120, { steps: 12 });
  await page.mouse.up();

  await expect(page.getByTestId('expanded-layer')).toHaveCount(0);
});

test.fixme('clic ouvre l overlay puis clic exterieur referme', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);
  const miniB = page.getByTestId('mini-B');
  await miniB.click({ force: true });
  await expect(page.getByTestId('expanded-card')).toBeVisible();
  await page.getByTestId('board').click({ position: { x: 12, y: 12 }, force: true });
  await expect(page.getByTestId('expanded-layer')).toHaveCount(0);
});

test.describe('mobile emulation', () => {
  test.use({
    viewport: { width: 393, height: 851 },
    hasTouch: true
  });

  test.fixme('mobile: ouverture/fermeture overlay', async ({ page }) => {
    await page.goto('/index.html');
    await page.waitForTimeout(200);

    const miniB = page.getByTestId('mini-B');
    await expect(miniB).toBeVisible();

    await miniB.click({ force: true });
    await expect(page.getByTestId('expanded-card')).toBeVisible();

    await page.getByTestId('board').click({ position: { x: 16, y: 16 }, force: true });
    await expect(page.getByTestId('expanded-layer')).toHaveCount(0);
  });
});
