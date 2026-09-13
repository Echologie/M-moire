(() => {
  "use strict";

  function escapeHtml(value) {
    return value
      .replaceAll("&", "&amp;")
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll('"', "&quot;")
      .replaceAll("'", "&#039;");
  }

  function inlineMarkdown(value) {
    return value
      .replace(/\`([^\`]+)\`/g, "<code>$1</code>")
      .replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  }

  function markdownToHtml(source) {
    const input = escapeHtml(source).replace(/\r\n?/g, "\n");
    if (!input.trim()) return "";

    const lines = input.split("\n");
    const html = [];
    let paragraph = [];
    let list = [];

    const flushParagraph = () => {
      if (paragraph.length) {
        // Leading spaces encode the experimental layout of a proof. A block
        // per line also keeps wrapped text aligned with its scope on mobile.
        const content = paragraph.some(line => /^ +/.test(line))
          ? paragraph.map(line => {
              const indent = line.match(/^ */)[0].length;
              return `<span class="proof-line" style="display:block;padding-left:${indent}ch">${inlineMarkdown(line.slice(indent))}</span>`;
            }).join("")
          : inlineMarkdown(paragraph.join("<br>"));
        html.push("<p>" + content + "</p>");
        paragraph = [];
      }
    };

    const flushList = () => {
      if (list.length) {
        html.push("<ul>" + list.map(item => "<li>" + inlineMarkdown(item) + "</li>").join("") + "</ul>");
        list = [];
      }
    };

    for (const line of lines) {
      if (!line.trim()) {
        flushParagraph();
        flushList();
        continue;
      }

      const listMatch = line.match(/^\s*-\s+(.+)$/);
      if (listMatch) {
        flushParagraph();
        list.push(listMatch[1]);
        continue;
      }

      const headingMatch = line.match(/^(#{1,3})\s+(.+)$/);
      if (headingMatch) {
        flushParagraph();
        flushList();
        const level = headingMatch[1].length;
        html.push("<h" + level + ">" + inlineMarkdown(headingMatch[2]) + "</h" + level + ">");
        continue;
      }

      flushList();
      paragraph.push(line);
    }

    flushParagraph();
    flushList();
    return html.join("");
  }

  class RichText extends HTMLElement {
    static get observedAttributes() {
      return ["content"];
    }

    connectedCallback() {
      this.render();
    }

    attributeChangedCallback() {
      this.render();
    }

    render() {
      const source = this.getAttribute("content") || "";
      this.innerHTML = markdownToHtml(source);

      if (typeof window.renderMathInElement === "function") {
        window.renderMathInElement(this, {
          delimiters: [
            { left: "$$", right: "$$", display: true },
            { left: "$", right: "$", display: false }
          ],
          throwOnError: false,
          strict: "ignore"
        });
      }
    }
  }

  if (!customElements.get("rich-text")) {
    customElements.define("rich-text", RichText);
  }
})();
