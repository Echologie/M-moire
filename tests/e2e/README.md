# Tests E2E

Lancement:

```bash
npm run test:e2e:install
npm run test:e2e
```

Le serveur statique `docs/` est lance automatiquement via Playwright.
Le script `test:e2e` applique automatiquement un patch de compatibilité Firefox
pour les environnements Guix (wrapper + `LD_LIBRARY_PATH`).
