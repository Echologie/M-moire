"""A green deployment must not hide a missing or outdated application file."""

import importlib.util
from pathlib import Path
import unittest
from urllib.error import HTTPError
from urllib.parse import urlparse

spec = importlib.util.spec_from_file_location(
    "published_site", Path(__file__).parents[1] / "scripts/check-published-site.py"
)
published = importlib.util.module_from_spec(spec)
spec.loader.exec_module(published)


class PublishedSiteTests(unittest.TestCase):
    def setUp(self):
        self.files = {"index.html": b"<main>Regards</main>", "sliders.js": b"new sliders", "data/bank.json": b'{"version":2}'}
        self.expected = {path: published.digest(content) for path, content in self.files.items()}

    def fetch(self, url):
        return self.files[urlparse(url).path.removeprefix("/M-moire/")]

    def test_accepts_the_tested_files_on_a_project_url(self):
        self.assertEqual(published.mismatches("https://example.org/M-moire/", self.expected, self.fetch), [])

    def test_rejects_old_code_and_data_even_when_homepage_is_correct(self):
        self.files["sliders.js"] = b"old sliders"
        self.files["data/bank.json"] = b'{"version":1}'
        errors = published.mismatches("https://example.org/M-moire", self.expected, self.fetch)
        self.assertEqual(len(errors), 2)
        self.assertTrue(any(error.startswith("sliders.js :") for error in errors))
        self.assertTrue(any(error.startswith("data/bank.json :") for error in errors))

    def test_rejects_a_404(self):
        def unavailable(url):
            raise HTTPError(url, 404, "Not Found", {}, None)

        errors = published.mismatches("https://example.org/M-moire/", self.expected, unavailable)
        self.assertEqual(len(errors), len(self.files))
        self.assertTrue(all("404" in error for error in errors))


if __name__ == "__main__":
    unittest.main()
