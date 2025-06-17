from ui.main_ui import main
from dotenv import load_dotenv, find_dotenv
import os

output_root = "converted_result"
if not os.path.exists(output_root):
    os.makedirs(output_root)
    
if __name__ == "__main__":
    load_dotenv(override=True)
    main()