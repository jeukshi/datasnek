class WindowController extends HTMLElement {
    constructor() {
        super();
    }

    static get observedAttributes() {
        return ['location', 'reload'];
    }

    attributeChangedCallback(name, oldValue, newValue) {
        if (name === 'location' && newValue) {
            window.location.href = newValue;
        } else if (name === 'reload' && newValue) {
            window.location.reload();
        }
    }
}

customElements.define('window-controller', WindowController);
