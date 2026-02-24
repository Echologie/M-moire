const { test, expect } = require('@playwright/test');

async function miniPressAndRelease(locator) {
  const box = await locator.boundingBox();
  if (!box) throw new Error('miniature introuvable');
  const x = box.x + box.width / 2;
  const y = box.y + box.height / 2;
  await locator.dispatchEvent('mousedown', { button: 0, clientX: x, clientY: y });
  await locator.dispatchEvent('mouseup', { button: 0, clientX: x, clientY: y });
}

async function expectCloseWithAnimation(page) {
  await expect(page.getByTestId('expanded-layer')).toBeVisible();
  await page.waitForTimeout(320);
  await expect(page.getByTestId('expanded-layer')).toHaveCount(0);
}

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

test('clic ouvre l overlay puis clic exterieur referme', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);
  const miniB = page.getByTestId('mini-B');
  await miniPressAndRelease(miniB);
  await expect(page.getByTestId('expanded-card')).toBeVisible();
  await page.getByTestId('expanded-layer').click({ position: { x: 12, y: 12 }, force: true });
  await expectCloseWithAnimation(page);
});

test('clic dans la fiche ouverte la referme', async ({ page }) => {
  await page.goto('/index.html');
  await page.waitForTimeout(200);
  const miniC = page.getByTestId('mini-C');
  await miniPressAndRelease(miniC);
  await expect(page.getByTestId('expanded-card')).toBeVisible();
  await page.getByTestId('expanded-card').click({ position: { x: 20, y: 20 }, force: true });
  await expectCloseWithAnimation(page);
});

test.describe('mobile emulation', () => {
  test.use({
    viewport: { width: 393, height: 851 },
    hasTouch: true
  });

  test('mobile: ouverture/fermeture overlay', async ({ page }) => {
    await page.goto('/index.html');
    await page.waitForTimeout(200);

    const miniB = page.getByTestId('mini-B');
    await expect(miniB).toBeVisible();

    await miniPressAndRelease(miniB);
    await expect(page.getByTestId('expanded-card')).toBeVisible();

    await page.getByTestId('expanded-layer').click({ position: { x: 16, y: 16 }, force: true });
    await expectCloseWithAnimation(page);
  });
});
