from ui.main_ui import main
from dotenv import load_dotenv, find_dotenv
import os

if __name__ == "__main__":
    load_dotenv(override=True)
    main()