import requests
from bs4 import BeautifulSoup
import pandas as pd
from datetime import date as dt_date

class SwapDiDownloader:
    def __init__(self, base_url=None, verbose=True):
        # Initialize with optional base_url and verbosity flag
        self.verbose = verbose
        self.url_template = base_url or (
            "https://www2.bmf.com.br/pages/portal/bmfbovespa/lumis/lum-taxas-referenciais-bmf-ptBR.asp?Data={data_1}&Data1={data_2}&slcTaxa=PRE"
            )

    def _is_weekend(self, date):
        # Check if the given date is a weekend (Saturday or Sunday)
        return date.weekday() >= 5

    def _fetch_html(self, date):
        # Build the URL for the given date and fetch the HTML content
        url = self.url_template.format(data_1 = date.strftime("%d/%m/%Y"), data_2=date.strftime("%Y%m%d"))
        try:
            response = requests.get(url)
            response.raise_for_status()
            return response.text
        except requests.RequestException as e:
            # Print error if verbose and return None on failure
            if self.verbose:
                print(f"Erro ao acessar {url}: {e}")
            return None

    def _parse_table(self, html, date):
        # Parse the HTML table and extract the data into a DataFrame
        soup = BeautifulSoup(html, "html.parser")
        td_nodes = soup.select("table#tb_principal1 td")

        if not td_nodes:
            # No data found for the given date
            if self.verbose:
                print(f"Nenhum dado encontrado para a data: {date}")
            return pd.DataFrame()

        td_text = [td.get_text(strip=True) for td in td_nodes]
        registros = []

        # Extract data in groups of 3 columns: DiasCorridos, TX_252, TX_360
        for i in range(0, len(td_text), 3):
            try:
                dias_corridos = int(td_text[i].replace(".", ""))
                tx_252 = float(td_text[i + 1].replace(".", "").replace(",", "."))
                tx_360 = float(td_text[i + 2].replace(".", "").replace(",", "."))
                registros.append([date, dias_corridos, tx_252, tx_360])
            except (ValueError, IndexError):
                # Skip rows with invalid data
                continue

        return pd.DataFrame(registros, columns=["Data", "DiasCorridos", "TX_252", "TX_360"])

    def download(self, date):
        # Main method to download and parse data for a given date
        if isinstance(date, str):
            # Convert string date to datetime.date
            date = dt_date.fromisoformat(date)

        if self._is_weekend(date):
            # Skip weekends
            if self.verbose:
                print(f"Fim de semana - data: {date}")
            return pd.DataFrame()

        if self.verbose:
            print(f"Processando a data: {date}")

        html = self._fetch_html(date)
        if html is None:
            return pd.DataFrame()

        return self._parse_table(html, date)
