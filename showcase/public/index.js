import { Elm } from './../src/Main.elm'
import paackSvgIconSprite from 'paack-ui-assets/js/paackSvgIconSprite'

const template = Object.assign(document.createElement('template'), {
  innerHTML: `
    <style>
      .text {
        white-space: nowrap;
      }

      .text--overflown {
        cursor: pointer;
        text-overflow: ellipsis;
        overflow: hidden;
        display: block;
      }

      .tooltip {
        outline: 0;
        position: absolute;
        top: calc(100% + 8px);
        display: block;
        z-index: 1;
        background: rgba(228, 228, 228);
        border-radius: 8px;
        width: 100%;
        box-sizing: content-box;
        left: -12px;
        opacity: 0;
        width: 0;
        clip: rect(0, 0, 0, 0);
      }

      .tooltip::before {
        content: '';
        height: 0;
        width: 0;
        border: 8px solid transparent;
        border-bottom-color: rgba(228, 228, 228);
        position: absolute;
        top: -16px;
        right: 16px;
      }

      .tooltip::after {
        content: '';
        height: 8px;
        width: 100%;
        position: absolute;
        top: -8px;
        right: 0;
        background: transparent;
      }

      .tooltip:hover,
      .tooltip:focus,
      .text--overflown:focus + .tooltip,
      .text--overflown:hover + .tooltip {
        opacity: 1;
        width: 100%;
        padding: 8px 16px;
        clip: auto;
      }
    </style>
    <span class="text"></span>
  `,
})

class EllipsizableText extends HTMLElement {
  constructor() {
    super()
    this.attachShadow({ mode: 'open' })
    this.shadowRoot.appendChild(template.content.cloneNode(true))
  }

  connectedCallback() {
    const { textContent } = this
    const text = this.shadowRoot.querySelector('.text')

    text.textContent = textContent

    if (text.offsetWidth > this.parentElement.offsetWidth) {
      text.classList.add('text--overflown')
      text.setAttribute('tabIndex', 0)

      const tooltip = text.cloneNode(true)

      tooltip.setAttribute('tabIndex', 0)

      tooltip.className = 'tooltip'
      this.shadowRoot.appendChild(tooltip)
    }
  }
}

window.customElements.define('ellipsizable-text', EllipsizableText)

const app = Elm.Main.init({
  node: document.getElementById('root'),
})
