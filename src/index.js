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
 * Start the App!
 */
Elm.Main.init();
