const { defineConfig, devices } = require('@playwright/test');

const runtimeLibPath =
  '/gnu/store/z1kd5nisk0mqacsyrdbzm0cbp6wvgsrs-profile/lib:/gnu/store/zzpbp6rr43smwxzvzd4qd317z5j7qblj-gcc-11.4.0-lib/lib';

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
    command: 'python3 -m http.server 4173 --directory docs',
    url: 'http://127.0.0.1:4173/index.html',
    reuseExistingServer: true,
    timeout: 30000
  },
  projects: [
    {
      name: 'firefox',
      use: {
        ...devices['Desktop Firefox'],
        browserName: 'firefox',
        launchOptions: {
          env: {
            ...process.env,
            LD_LIBRARY_PATH: runtimeLibPath
          }
        }
      }
    }
  ]
});
