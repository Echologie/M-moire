const { defineConfig, devices } = require('@playwright/test');

module.exports = defineConfig({
  testDir: './tests/e2e',
  timeout: 30000,
  workers: 1,
  retries: 0,
  reporter: [['list']],
  use: {
    baseURL: 'http://127.0.0.1:4173',
    trace: 'retain-on-failure',
    video: 'retain-on-failure',
    screenshot: 'only-on-failure'
  },
  webServer: {
    command: 'python3 -m http.server 4173 --directory site',
    url: 'http://127.0.0.1:4173/index.html',
    reuseExistingServer: true,
    timeout: 30000
  },
  projects: [
    {
      name: 'firefox',
      testIgnore: '**/*.touch.spec.js',
      use: {
        ...devices['Desktop Firefox'],
        browserName: 'firefox'
      }
    },
    {
      name: 'chromium',
      testMatch: '**/survey-gestures.spec.js',
      use: { ...devices['Desktop Chrome'], browserName: 'chromium' }
    },
    {
      name: 'chromium-touch',
      testMatch: '**/*.touch.spec.js',
      use: { browserName: 'chromium', viewport: { width: 390, height: 844 }, isMobile: true, hasTouch: true }
    }
  ]
});
