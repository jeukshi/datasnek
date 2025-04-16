import requests
import uuid
import time
import re
import random
import threading
import sseclient
import html
import html.parser
import json
import argparse
import sys
from typing import Tuple, Dict, List, Optional
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('snake-bot')

BOARD_HTML_RE = re.compile(r'<div[^>]+id=["\']board["\']', re.IGNORECASE)
SNEK_GAME_BOARD_RE = re.compile(r'<snek-game-board\s+([^>]*)>', re.IGNORECASE)

class BoardParser(html.parser.HTMLParser):
    """Parse the HTML board representation to extract snake and food positions"""

    def __init__(self, bot_name):
        super().__init__()
        self.bot_name = bot_name
        self.in_board = False
        self.current_row = 0
        self.current_col = 0
        self.food_positions = []  # Changed to list to store multiple food positions
        self.snake_cells = []
        self.my_head_position = None
        self.other_snake_positions = []
        self.board_size = 0

    def handle_starttag(self, tag, attrs):
        attrs_dict = dict(attrs)

        if tag == 'div' and 'class' in attrs_dict:
            class_val = attrs_dict['class']

            # Start of board
            if 'board' in class_val and 'id' in attrs_dict and attrs_dict['id'] == 'board':
                self.in_board = True

            # Board item
            elif self.in_board and 'board-item' in class_val:
                # Food
                if 'food' in class_val:
                    self.food_positions.append((self.current_col, self.current_row))

                # Snake cell
                elif 'snake' in class_val:
                    # Check if this is a head (contains nameplate)
                    is_head = False
                    self.snake_cells.append((self.current_col, self.current_row))

                self.current_col += 1

            # New row in the board
            elif self.in_board and 'board-container' in class_val:
                self.current_row += 1
                self.current_col = 0
                # Update board size based on the max row we've seen
                self.board_size = max(self.board_size, self.current_row)

    def handle_data(self, data):
        # If we find our bot name, the previous cell was our head
        if data.strip() == self.bot_name and self.snake_cells:
            self.my_head_position = self.snake_cells[-1]


class SnakeBot:
    # Fun name templates for bots
    NAME_TEMPLATES = [
        "Claude the {adjective}",
        "{adjective} Claude",
        "Claude {number}",
        "Sir Claude {adjective}",
        "Claude of {place}",
        "Claude {adjective} {number}",
    ]

    ADJECTIVES = [
        "Great", "Mighty", "Swift", "Wise", "Cunning", "Brave", "Bold",
        "Sneaky", "Slithery", "Hungry", "Quick", "Nimble", "Clever",
        "Fierce", "Agile", "Daring", "Legendary", "Unstoppable", "Magnificent"
    ]

    PLACES = [
        "Snakeland", "Pythonia", "Slithertown", "Viperwood", "Cobraville",
        "Serpentia", "Fangtopia", "Scaleshire", "Anacondville", "Reptilia"
    ]

    def __init__(self, base_url: str = "http://localhost:8080", bot_name: str = None, bot_index: int = 0):
        self.base_url = base_url

        # Generate a fun name if not provided
        if bot_name is None:
            self.bot_name = self.generate_fun_name(bot_index)
        else:
            self.bot_name = bot_name

        self.bot_id = str(uuid.uuid4())
        self.cookies = {}
        self.direction = "R"  # Start by moving right
        self.board_size = 20  # Default, will be updated
        self.head_position = None
        self.food_positions = []  # Changed to list to store multiple food positions
        self.running = False
        self.transmittal_thread = None
        self.login_success = False
        self.deaths = 0  # Track number of deaths/respawns
        self.bot_index = bot_index  # Store the bot's index for logging

        # Customize logger for this bot instance
        self.logger = logging.getLogger(f'snake-bot-{self.bot_index}')
        self.logger.setLevel(logging.INFO)

    @classmethod
    def generate_fun_name(cls, index):
        """Generate a fun name for the bot"""
        template = random.choice(cls.NAME_TEMPLATES)

        # Replace placeholders in the template
        name = template.format(
            adjective=random.choice(cls.ADJECTIVES),
            place=random.choice(cls.PLACES),
            number=index + 1
        )

        return name

    def create_session(self):
        """Create a session with necessary cookies"""
        # Create cookies directly
        self.cookies = {
            'datasnek-name': self.bot_name,
            'datasnek-id': self.bot_id
        }

        self.logger.info(f"Created session as {self.bot_name} with ID {self.bot_id}")
        self.login_success = True
        return True

    def start_game(self):
        """Signal the server that we want to play"""
        play_url = f"{self.base_url}/api/play"

        try:
            self.logger.info(f"REQUEST: POST {play_url} with cookies {self.cookies}")
            response = requests.post(
                play_url,
                cookies=self.cookies,
                stream=True
            )
            self.logger.info(f"RESPONSE: Status {response.status_code}")
            self.logger.info("Started playing")
            return True
        except Exception as e:
            self.logger.error(f"Failed to start game: {e}")
            return False

    def change_direction(self, direction: str):
        """Change the snake's direction"""
        if direction not in ["U", "D", "L", "R"]:
            self.logger.warning(f"Invalid direction: {direction}")
            return False

        # Don't allow moving in the opposite direction
        opposites = {"U": "D", "D": "U", "L": "R", "R": "L"}
        if direction == opposites.get(self.direction):
            return False

        change_dir_url = f"{self.base_url}/api/game/change-direction/{direction}"

        try:
            self.logger.debug(f"REQUEST: GET {change_dir_url} with cookies {self.cookies}")
            response = requests.get(
                change_dir_url,
                cookies=self.cookies,
                stream=True
            )
            self.logger.debug(f"RESPONSE: Status {response.status_code}")
            self.direction = direction
            self.logger.debug(f"Changed direction to {direction}")
            return True
        except Exception as e:
            self.logger.error(f"Failed to change direction: {e}")
            return False

    def find_closest_food(self):
        """Find the closest food item to the snake's head"""
        if not self.head_position or not self.food_positions:
            return None

        head_x, head_y = self.head_position
        closest_food = None
        min_distance = float('inf')

        for food_pos in self.food_positions:
            food_x, food_y = food_pos
            # Manhattan distance
            distance = abs(head_x - food_x) + abs(head_y - food_y)

            if distance < min_distance:
                min_distance = distance
                closest_food = food_pos

        return closest_food

    def decide_move(self):
        """Smart AI to decide the next move based on closest food"""
        if not self.head_position:
            # If we don't know our position, make a random move
            return random.choice(["U", "D", "L", "R"])

        # Find the closest food item
        closest_food = self.find_closest_food()

        if not closest_food:
            # If no food is available, make a random move
            return random.choice(["U", "D", "L", "R"])

        head_x, head_y = self.head_position
        food_x, food_y = closest_food

        # Possible moves and their new positions
        moves = {
            "U": (head_x, head_y - 1),
            "D": (head_x, head_y + 1),
            "L": (head_x - 1, head_y),
            "R": (head_x + 1, head_y)
        }

        # Don't move into walls
        valid_moves = []
        for direction, (new_x, new_y) in moves.items():
            if 0 <= new_x <= self.board_size and 0 <= new_y <= self.board_size:
                valid_moves.append(direction)

        if not valid_moves:
            return random.choice(["U", "D", "L", "R"])  # Last resort

        # Prefer moves that get us closer to food
        best_move = None
        best_distance = float('inf')

        for direction in valid_moves:
            new_x, new_y = moves[direction]
            # Manhattan distance to food
            distance = abs(new_x - food_x) + abs(new_y - food_y)

            if distance < best_distance:
                best_distance = distance
                best_move = direction

        # 20% chance to make a random move for unpredictability
        if random.random() < 0.2:
            return random.choice(valid_moves)

        return best_move or random.choice(valid_moves)

    def respawn(self):
        """Respawn the bot after dying"""
        # Wait a few seconds before respawning
        self.deaths += 1
        respawn_delay = random.uniform(2.0, 5.0)
        self.logger.info(f"Waiting {respawn_delay:.2f} seconds before respawning... (Death #{self.deaths})")
        time.sleep(respawn_delay)

        # Reset some state
        self.direction = "R"  # Reset direction to start moving right
        self.head_position = None
        self.food_positions = []  # Reset food positions

        # Join the game again
        success = self.start_game()
        if success:
            self.logger.info(f"Successfully respawned! (Respawn #{self.deaths})")
        else:
            self.logger.error("Failed to respawn")

    def parse_attributes(self, tag_string: str) -> Dict[str, str]:
        """Parse attributes from a self-contained HTML tag string"""
        attr_re = re.compile(r'(\w+)="([^"]*)"')
        return {key: value for key, value in attr_re.findall(tag_string)}

    def process_board(self, html_content: str):
        """Process the board HTML or Web Component to extract game state"""
        match = SNEK_GAME_BOARD_RE.search(html_content)
        if match:
            # Web Component format
            attrs_raw = match.group(1)
            attrs = self.parse_attributes(attrs_raw)

            self.board_size = int(attrs.get("board-size", self.board_size))

            try:
                # Parse food positions
                food_raw = attrs.get("food", "[]")
                self.food_positions = json.loads(food_raw)

                # Parse sneks
                sneks_raw = attrs.get("sneks", "[]")
                sneks_json = html.unescape(sneks_raw)
                sneks = json.loads(sneks_json)

                # Find this bot's head
                for snek in sneks:
                    if snek.get("username") == self.bot_name:
                        self.head_position = tuple(snek["headOfSnek"])
                        break
                else:
                    # Bot has died
                    if self.head_position:
                        self.logger.info(f"Bot {self.bot_name} died! Will respawn shortly...")
                        self.head_position = None
                        threading.Thread(target=self.respawn, daemon=True).start()
                        return

                if self.head_position:
                    next_move = self.decide_move()
                    if next_move:
                        self.change_direction(next_move)

            except Exception as e:
                self.logger.error(f"Error parsing snek-game-board data: {e}")

        else:
            # Fallback to original parser
            parser = BoardParser(self.bot_name)
            parser.feed(html_content)

            if parser.board_size > 0:
                self.board_size = parser.board_size

            self.food_positions = parser.food_positions
            previous_head_position = self.head_position
            self.head_position = parser.my_head_position

            if previous_head_position and not self.head_position:
                self.logger.info(f"Bot {self.bot_name} died! Will respawn shortly...")
                threading.Thread(target=self.respawn, daemon=True).start()
                return

            if self.head_position:
                next_move = self.decide_move()
                if next_move:
                    self.change_direction(next_move)

    def listen_for_updates(self):
        """Listen for game state updates via SSE"""
        transmittal_url = f"{self.base_url}/api/transmittal"

        try:
            self.logger.info(f"REQUEST: GET {transmittal_url} with cookies {self.cookies}")
            response = requests.get(
                transmittal_url,
                cookies=self.cookies,
                stream=True,
                headers={'Accept': 'text/event-stream'}
            )
            self.logger.info(f"RESPONSE: Status {response.status_code}")

            client = sseclient.SSEClient(response)
            self.logger.info("Started listening for game updates")

            for event in client.events():
                if not self.running:
                    break

                #if event.data and BOARD_HTML_RE.search(event.data):
                if event.data and '<div id="board"' in event.data:
                    self.logger.debug(f"Received board update: {event.data[:100]}...")
                    self.process_board(event.data)

        except Exception as e:
            self.logger.error(f"Error listening for updates: {e}")

    def run(self):
        """Main bot loop"""
        # Create session directly with cookies
        self.create_session()

        # Start playing
        if not self.start_game():
            self.logger.error("Failed to start game, exiting")
            return

        # Start listening for updates in a separate thread
        self.running = True
        self.transmittal_thread = threading.Thread(target=self.listen_for_updates)
        self.transmittal_thread.daemon = True
        self.transmittal_thread.start()

        self.logger.info(f"Bot {self.bot_name} is running")

        # Keep running until stopped externally
        while self.running:
            time.sleep(0.1)

    def stop(self):
        """Stop the bot"""
        self.running = False
        if self.transmittal_thread:
            self.transmittal_thread.join(timeout=2)
        self.logger.info(f"Bot {self.bot_name} stopped")


class BotManager:
    """Manager for multiple snake bots"""
    def __init__(self, num_bots=1, server_url="http://localhost:8080"):
        self.num_bots = num_bots
        self.server_url = server_url
        self.bots = []
        self.running = False

    def start_bots(self):
        """Start all bots"""
        self.running = True

        # Create and start each bot in its own thread
        for i in range(self.num_bots):
            bot = SnakeBot(base_url=self.server_url, bot_index=i)
            self.bots.append(bot)

            # Create thread for this bot
            bot_thread = threading.Thread(target=bot.run)
            bot_thread.daemon = True
            bot_thread.start()

            # Small delay between starting bots to prevent server overload
            time.sleep(0.5)

        logger.info(f"Started {self.num_bots} bots")

        # Print all bot names
        for i, bot in enumerate(self.bots):
            logger.info(f"Bot {i}: {bot.bot_name}")

    def stop_bots(self):
        """Stop all bots"""
        self.running = False

        for bot in self.bots:
            bot.stop()

        logger.info("All bots stopped")


def parse_arguments():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(description='Snake Game Bot')
    parser.add_argument('--bots', type=int, default=1, help='Number of bots to run (default: 1)')
    parser.add_argument('--server', type=str, default='http://localhost:3000', help='Server URL (default: http://localhost:8080)')
    parser.add_argument('--debug', action='store_true', help='Enable debug logging')

    return parser.parse_args()


if __name__ == "__main__":
    # Parse command line arguments
    args = parse_arguments()

    # Set logging level
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
        logger.setLevel(logging.WARNING)

    # Log startup information
    logger.info(f"Starting {args.bots} snake bots connecting to {args.server}")

    # Create and start bot manager
    manager = BotManager(num_bots=args.bots, server_url=args.server)
    manager.start_bots()

    try:
        # Keep main thread alive
        while manager.running:
            time.sleep(1)
    except KeyboardInterrupt:
        logger.info("Stopping due to keyboard interrupt")
        manager.stop_bots()

    logger.info("Snake bot program exited")
