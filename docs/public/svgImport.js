import svg from './icons.html';

console.log(svg);
class SvgImport extends HTMLElement {
  constructor() {
    // Always call super first in constructor
    super();
  }

  static get observedAttributes() {
    return [];
  }

  connectedCallback() {
    this.innerHTML = svg;
  }
}

window.customElements.define('svg-import', SvgImport);
