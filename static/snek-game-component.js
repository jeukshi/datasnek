class SnekGameBoard extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this.prevSneks = [];
    this.prevFood = [];
    this.cellsMap = new Map(); // Store references to cells by coordinates
    this.boardSize = 0;
    this.boardInitialized = false;
  }

  static get observedAttributes() {
    return ['board-size', 'food', 'sneks', 'username', 'anonymous'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === 'anonymous') {
      // Handle boolean attribute conversion
      this.isAnonymous = newValue !== null && newValue !== 'false';
    }

    if (name === 'board-size' && oldValue !== newValue) {
      // Board size changed, need to rebuild the entire board
      this.boardSize = parseInt(newValue, 10) || 20;
      this.buildBoard();
    } else if (name === 'food' || name === 'sneks') {
      // Only update the game state without rebuilding the board
      this.updateGameState();
    } else {
      // For other attribute changes like username or anonymous
      this.updateGameState();
    }
  }

  connectedCallback() {
    // Set default values
    this.isAnonymous = this.hasAttribute('anonymous') &&
                       this.getAttribute('anonymous') !== 'false';

    // Initialize board size
    this.boardSize = parseInt(this.getAttribute('board-size'), 10) || 20;

    // Build the initial board structure
    this.buildBoard();
  }

  // Helper method to check if a snek belongs to the current user
  isCurrentUser(userName) {
    // Get username directly from attribute
    const currentUserName = this.getAttribute('username');
    if (!currentUserName || !userName) return false;

    // Normalize both strings for comparison (trim whitespace, case insensitive)
    const normalizedCurrent = currentUserName.trim().toLowerCase();
    const normalizedUserName = userName.trim().toLowerCase();

    return normalizedCurrent === normalizedUserName;
  }

  // Helper method to calculate opacity based on grace period
  calculateGracePeriodOpacity(gracePeriod) {
    if (gracePeriod <= 0) {
      return 1.0; // Full opacity when not in grace period
    }

    if (gracePeriod >= 5) {
      return 0.5; // 50% opacity for grace period >= 5
    }

    // Gradually increase opacity in the last 5 ticks
    // gracePeriod 1 -> 0.9 opacity
    // gracePeriod 2 -> 0.8 opacity
    // gracePeriod 3 -> 0.7 opacity
    // gracePeriod 4 -> 0.6 opacity
    return 0.5 + (0.1 * (5 - gracePeriod));
  }

  // Build the entire board structure (only when board size changes)
  buildBoard() {
    // Clear existing content
    this.shadowRoot.innerHTML = '';
    this.cellsMap.clear();

    // Determine if this is a large board that should use compact styling
    const isLargeBoard = this.boardSize > 150;

    // Add styles
    const style = document.createElement('style');
    style.textContent = this.getStyles(isLargeBoard);
    this.shadowRoot.appendChild(style);

    // Create board container
    const board = document.createElement('div');
    board.classList.add('board');

    // Generate rows and cells
    for (let r = 0; r <= this.boardSize; r++) {
      const row = document.createElement('div');
      row.classList.add('board-container');

      for (let c = 0; c <= this.boardSize; c++) {
        const cell = document.createElement('div');
        cell.classList.add('board-item');

        // Add compact class for large boards
        if (isLargeBoard) {
          cell.classList.add('compact');
        }

        // Store reference to this cell
        const coordKey = `${c},${r}`;
        this.cellsMap.set(coordKey, cell);

        row.appendChild(cell);
      }

      board.appendChild(row);
    }

    this.shadowRoot.appendChild(board);
    this.boardInitialized = true;

    // Update game state after building the board
    this.updateGameState();
  }

  // Update only the necessary cells based on changes in food and snake positions
  updateGameState() {
    if (!this.boardInitialized) return;

    let food = [];
    let sneks = [];
    const isLargeBoard = this.boardSize > 150;

    try {
      food = JSON.parse(this.getAttribute('food')) || [];
    } catch (e) {
      console.warn('Invalid JSON in food attribute:', e);
    }

    try {
      sneks = JSON.parse(this.getAttribute('sneks')) || [];
    } catch (e) {
      console.warn('Invalid JSON in sneks attribute:', e);
    }

    // Create sets of all current positions to simplify comparisons
    const currentFoodPositions = new Set(food.map(([x, y]) => `${x},${y}`));
    const currentSnekPositions = new Map();
    const currentHeadPositions = new Map();

    // Process all sneks to get their positions and metadata
    sneks.forEach(snek => {
      const { color, headOfSnek, restOfSnek, gracePeriod = 0 } = snek;
      const userName = snek.user || snek.username || snek.name;
      const opacity = this.calculateGracePeriodOpacity(gracePeriod);

      // Store head position with metadata including grace period info
      if (headOfSnek) {
        const headKey = `${headOfSnek[0]},${headOfSnek[1]}`;
        currentHeadPositions.set(headKey, { color, userName, opacity });
      }

      // Store body positions with color and opacity
      if (restOfSnek) {
        restOfSnek.forEach(([x, y]) => {
          const posKey = `${x},${y}`;
          currentSnekPositions.set(posKey, { color, opacity });
        });
      }
    });

    // Create sets of previous positions
    const prevFoodPositions = new Set(this.prevFood.map(([x, y]) => `${x},${y}`));
    const prevSnekPositions = new Map();
    const prevHeadPositions = new Map();

    this.prevSneks.forEach(snek => {
      const { color, headOfSnek, restOfSnek, gracePeriod = 0 } = snek;
      const userName = snek.user || snek.username || snek.name;
      const opacity = this.calculateGracePeriodOpacity(gracePeriod);

      if (headOfSnek) {
        const headKey = `${headOfSnek[0]},${headOfSnek[1]}`;
        prevHeadPositions.set(headKey, { color, userName, opacity });
      }

      if (restOfSnek) {
        restOfSnek.forEach(([x, y]) => {
          const posKey = `${x},${y}`;
          prevSnekPositions.set(posKey, { color, opacity });
        });
      }
    });

    // 1. Clean up cells that are no longer food or snake

    // Clear old food positions
    for (const posKey of prevFoodPositions) {
      if (!currentFoodPositions.has(posKey) && this.cellsMap.has(posKey)) {
        const cell = this.cellsMap.get(posKey);
        cell.classList.remove('food');
      }
    }

    // Clear old snake head positions
    for (const [posKey] of prevHeadPositions) {
      if (!currentHeadPositions.has(posKey) && this.cellsMap.has(posKey)) {
        const cell = this.cellsMap.get(posKey);
        // Remove nameplate if exists
        const nameplate = cell.querySelector('.nameplate, .nameplate-me');
        if (nameplate) {
          cell.removeChild(nameplate);
        }

        // Only remove snake class if it's not part of a snake body now
        if (!currentSnekPositions.has(posKey)) {
          cell.classList.remove('snake');
          cell.style.backgroundColor = '';
          cell.style.opacity = ''; // Reset opacity
        }
      }
    }

    // Clear old snake body positions
    for (const [posKey] of prevSnekPositions) {
      if (!currentSnekPositions.has(posKey) && !currentHeadPositions.has(posKey) && this.cellsMap.has(posKey)) {
        const cell = this.cellsMap.get(posKey);
        cell.classList.remove('snake');
        cell.style.backgroundColor = '';
        cell.style.opacity = ''; // Reset opacity
      }
    }

    // 2. Add new food positions
    for (const posKey of currentFoodPositions) {
      if (this.cellsMap.has(posKey)) {
        const cell = this.cellsMap.get(posKey);
        // Check if it's not already marked as food (for refresh cases)
        if (!cell.classList.contains('food')) {
          cell.classList.add('food');

          // For large boards, we skip the border-radius in CSS
          if (isLargeBoard) {
            cell.classList.add('compact-food');
          }
        }
      }
    }

    // 3. Add new snake body positions with grace period transparency
    for (const [posKey, snekData] of currentSnekPositions) {
      if (this.cellsMap.has(posKey)) {
        const cell = this.cellsMap.get(posKey);
        const { color, opacity } = snekData;

        cell.classList.add('snake');
        cell.style.backgroundColor = color;
        cell.style.opacity = opacity.toString();
      }
    }

    // 4. Add new snake head positions with nameplates and grace period transparency
    for (const [posKey, metadata] of currentHeadPositions) {
      if (this.cellsMap.has(posKey)) {
        const cell = this.cellsMap.get(posKey);
        const { color, userName, opacity } = metadata;

        // Add snake styling with transparency
        cell.classList.add('snake');
        cell.style.backgroundColor = color;
        cell.style.opacity = opacity.toString();

        // Create nameplate if we have a user name
        if (userName) {
          // Check if this is the current user's snek
          const isCurrentUser = this.isCurrentUser(userName);

          // Only add nameplate if it doesn't exist or has changed
          let nameplate = cell.querySelector('.nameplate, .nameplate-me');

          if (!nameplate) {
            nameplate = document.createElement('div');
            cell.appendChild(nameplate);
          }

          // Update nameplate class and content
          nameplate.className = isCurrentUser ? 'nameplate-me' : 'nameplate';

          // For large boards, add compact class for smaller text
          if (isLargeBoard) {
            nameplate.classList.add('compact-nameplate');
          }

          nameplate.textContent = this.isAnonymous ? 'snek' : userName;
        }
      }
    }

    // Update previous states for next render
    this.prevSneks = JSON.parse(JSON.stringify(sneks));
    this.prevFood = JSON.parse(JSON.stringify(food));
  }

  getStyles(isLargeBoard = false) {
    return `
      .board {
        display: flex;
        flex-direction: column;
        aspect-ratio: 1 / 1;
        width: 100%;
        max-width: 600px;
        margin: 0 auto;
        background-color: #ffffff;
        border: ${isLargeBoard ? '0' : '1px solid rgba(0, 0, 0, 0.08)'};
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.06);
      }
      .board-container {
        display: flex;
        flex: 1;
      }
      .board-item {
        flex: 1;
        background-color: #eee;
        margin: ${isLargeBoard ? '0' : '1px'};
        position: relative;
      }
      .board-item.compact {
        margin: 0;
        border-radius: 0;
      }
      .board-item.food {
        background-color: red;
        border-radius: ${isLargeBoard ? '0' : '50%'};
      }
      .board-item.snake {
        background-color: green; /* Default color if none provided */
      }
      .nameplate {
        position: absolute;
        font-size: ${isLargeBoard ? '6px' : '10px'};
        background-color: rgba(255, 255, 255, 0.8);
        padding: ${isLargeBoard ? '1px 2px' : '2px 4px'};
        border-radius: 3px;
        white-space: nowrap;
        z-index: 10;
        transform: translate(-50%, -100%);
        top: -5px;
        left: 50%;
        color: #000;
        min-width: ${isLargeBoard ? '10px' : '20px'};
        text-align: center;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.2);
      }
      .nameplate-me {
        position: absolute;
        font-size: ${isLargeBoard ? '6px' : '10px'};
        background-color: rgba(255, 215, 0, 0.85);
        padding: ${isLargeBoard ? '1px 2px' : '2px 4px'};
        border-radius: 3px;
        white-space: nowrap;
        z-index: 10;
        transform: translate(-50%, -100%);
        top: -5px;
        left: 50%;
        font-weight: bold;
        color: #000;
        box-shadow: 0 0 4px rgba(0, 0, 0, 0.2);
        min-width: ${isLargeBoard ? '10px' : '20px'};
        text-align: center;
        border: 1px solid #FF8C00;
      }
    `;
  }
}

customElements.define('snek-game-board', SnekGameBoard);
