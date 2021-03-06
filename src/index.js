/**
 * A custom element for rendering a post's html content
 */
customElements.define(
  'post-content',
  class extends HTMLElement {
    constructor() {
      super();
      this._content = null;
    }

    set content(value) {
      this._content = value;
      this.updateContent();
    }

    connectedCallback() {
      this.updateContent();
    }

    updateContent() {
      this.innerHTML = this._content;
    }
  },
);

/**
 * Convert to a Theme string that Elm expects.
 * @param {boolean} prefersDark - If the browser prefers a dark theme
 * @return {string}
 */
function toTheme(prefersDark) {
  return prefersDark ? 'dark' : 'light';
}

const mediaQueryList = matchMedia('(prefers-color-scheme: dark)');

/**
 * Start the App!
 */
const app = Elm.Main.init({
  flags: {
    systemTheme: toTheme(mediaQueryList.matches),
    themePreference: localStorage.getItem("theme")
  },
});

/**
 * Listen for changes in the preffered system color scheme.
 */
mediaQueryList.addEventListener('change', event => {
  app.ports.currentTheme.send(toTheme(event.matches));
});

/**
 * Update user's theme preference
 */
app.ports.setThemePreference.subscribe(theme => {
  localStorage.setItem('theme', theme);
})
