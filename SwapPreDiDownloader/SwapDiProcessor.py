import os
import pandas as pd
import datetime
from concurrent.futures import ThreadPoolExecutor, as_completed
import SwapDiDownloader as DL

class SwapDiProcessor:
    def __init__(self, start_date : datetime.date = None, end_date: datetime.date=None, verbose_level: int = 0):

        self.start_date: datetime.date = start_date or (datetime.date.today() - datetime.timedelta(days=1000))
        self.end_date: datetime.date = end_date or (datetime.date.today() - datetime.timedelta(days=1))
        self.verbose: int = verbose_level

        self.outputSwapDiPre_dir = "./Database/SwapDiPre_daily_raw"
        self.outputSwapDiPre_fileMask: str = "swapdi_{data}.csv"

        self.dateCollection: list[datetime.date] = self.create_data_collection()

    def get_config(self) -> str:
        config_str = (
            "SwapDiProcessor Configuration:\n"
            f"  Start Date: {self.start_date}\n"
            f"  End Date: {self.end_date}\n"
            f"  Verbose Level: {self.verbose}\n"
            f"  Output Directory: {self.outputSwapDiPre_dir}\n"
            f"  Output File Mask: {self.outputSwapDiPre_fileMask}\n"
            f"  Dates to Process: {len(self.dateCollection)} dates"
        )
        return config_str

    def __str__(self) -> str:
        return ( self.get_config() )
    
    def set_date_range(self, start_date: datetime.date, end_date: datetime.date) -> None:
        """
        Set the start and end dates for processing and update the date collection.
        """
        self.start_date = start_date
        self.end_date = end_date
        self.dateCollection = self.create_data_collection()

    def log(self, message: str, level: int = 1) -> None:
        if self.verbose >= level:
            print(message)

    # def load_yearly_data(self):
    #     if os.path.exists(self.destination_path):
    #         return load(self.destination_path)
    #     return pd.DataFrame()

    # def save_yearly_data(self):
    #     os.makedirs(os.path.dirname(self.destination_path), exist_ok=True)
    #     dump(self.yearly_tbl, self.destination_path)

    # def load_daily_data(self, date):
    #     path = self.database_mask.format(date)
    #     if os.path.exists(path):
    #         return load(path)
    #     return pd.DataFrame()

    # def save_daily_data(self, date, data):
    #     path = self.database_mask.format(date)
    #     os.makedirs(os.path.dirname(path), exist_ok=True)
    #     dump(data, path)

    def check_raw_file_exists(self, date: datetime) -> bool:
        # Verifica se o arquivo já existe
        
        mFile = self.outputSwapDiPre_fileMask.format(data = date.strftime("%Y-%m-%d"))
        execution_dir = os.path.dirname(os.path.abspath(__file__))

        mFile_full = os.path.join(execution_dir, self.outputSwapDiPre_dir, mFile)
        if os.path.exists(mFile_full):
            return True
        else:
            return False
    
    def _is_weekend(self, date: datetime) -> bool:
        """
        Check if the given date is a weekend (Saturday or Sunday).
        """
        return date.weekday() >= 5

    def create_data_collection(self) -> list[datetime.date]:
        """
        Creates a list of dates between self.date_ini and self.date_fim
        for which the corresponding daily data file exists.
        """
        existing_dates = []
        for date in pd.date_range(start = self.start_date, end = self.end_date, freq='D'):
            # if date is weekday skip
            if self._is_weekend(date):
                self.log(f"Fim de semana - data: {date}", level=2) 
                continue
            # if day is alread downloaded skip
            elif self.check_raw_file_exists(date):
                self.log(f"Dia ja salvado em dados locais para {date}", level=2)
                continue
            self.log(f"Adicionando a data {date} na colecao.", level=2)

            # if day is not weekend and not already downloaded, add to list
            existing_dates.append(date)
        
        return existing_dates

    @staticmethod
    def get_num_cores() -> int:
        """
        Returns the number of CPU cores available on the system.

        Returns:
            int: The number of CPU cores detected. If detection fails, returns 1 as a fallback.
        """
        cores:int = 1
        try:
            # os.cpu_count() returns the number of logical CPUs in the system
            cores = os.cpu_count()
            if cores is None:
                # In rare cases, os.cpu_count() may return None
                cores = 1
        except Exception:
            # If any exception occurs during detection, fallback to 1
            cores = 1
        
        # self.log("") # (f"Detected number of cores: {get_num_cores()}")
    
        return cores

    def download_data(self) -> None:
        
        # Suponha que você já tenha isso:
        downloader = DL.SwapDiDownloader(verbose=False)
        os.makedirs(self.outputSwapDiPre_dir, exist_ok = True)

        # results = []
        with ThreadPoolExecutor(max_workers = self.get_num_cores()) as executor:
            future_to_date = {executor.submit(downloader.download, d): d for d in self.dateCollection}
            for future in as_completed(future_to_date):
                df = future.result()
                fileName = self.outputSwapDiPre_fileMask.format(data = future_to_date[future].strftime("%Y-%m-%d"))
                if not df.empty:
                    df.to_csv(os.path.join(self.outputSwapDiPre_dir, fileName), index = False)
                
                self.log(f"{fileName} processado.", level=1)
                    
                    # df.to_feather(os.path.join(output_dir, f"swapdi_{future_to_date[future]}.feather"))
        self.log("Download completo.", level=1)
                
    # def process_date(self, date):
    #   
    #     tbl = self.load_daily_data(date)
    #
    #     if tbl.empty:
    #         self.log(f"Baixando dados para {date}")
    #         tbl = self.download_data(date)
    #         if not tbl.empty:
    #             self.log(f"Salvando dados locais para {date}")
    #             self.save_daily_data(date, tbl)
    #         else:
    #             self.log(f"Nenhum dado encontrado para {date}")
    #             return
    #
    #     if not tbl.empty and 'Data' in tbl.columns:
    #         if not tbl['Data'].isin(self.yearly_tbl.get('Data', pd.Series([]))).all():
    #             self.yearly_tbl = pd.concat([self.yearly_tbl, tbl], ignore_index=True)
    #         else:
    #             self.log(f"Data {date} já existente no banco anual.")
