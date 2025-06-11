import SwapDiProcessor
import datetime

if __name__ == "__main__":

    obj = SwapDiProcessor.SwapDiProcessor(start_date = datetime.date(2020, 1, 1),
                                          end_date = datetime.date(2023, 8, 7),
                                          verbose_level=1)
    while True:
        print("\nSelect an action:")
        print("0. Define start and end date")
        print("1. Run")
        print("2. Print configuration")
        print("3. Exit")
        
        choice = input("\nEnter your choice: ").strip()
        if choice == "1":
            obj.download_data()
        elif choice == "2":
            print(obj)
        elif choice == "3":
            break
        elif choice == "0":
            try:
                # Ask user for new start and end dates
                start_input = input("Enter start date (YYYY-MM-DD): ").strip()
                end_input = input("Enter end date (YYYY-MM-DD): ").strip()
                
                # Convert input strings to date objects
                new_start = datetime.date.fromisoformat(start_input)
                new_end = datetime.date.fromisoformat(end_input)
                
                # Set new start and end dates
                obj.set_date_range(new_start, new_end)
                print(f"Start date set to {obj.start_date},"
                      f" end date set to {obj.end_date}."
                      f" Colection size: {len(obj.dateCollection)}")
            except Exception as e:
                print(f"\nError: {e}")
        else:
            print("Invalid choice. Please try again.")

    input("Press Enter to exit...")
    # downloader.download_all()