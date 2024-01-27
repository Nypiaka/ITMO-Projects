import requests
import html_to_json
import json as jsn
from bs4 import BeautifulSoup

headers = {
    'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/117.0',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
    'Connection': 'keep-alive',
    'Upgrade-Insecure-Requests': '1',
    'Sec-Fetch-Dest': 'document',
    'Sec-Fetch-Mode': 'navigate',
    'Sec-Fetch-Site': 'none',
    'Sec-Fetch-User': '?1'
}


def parsePage(link, proxies):
    reqs = requests.get(link, headers=headers, proxies=proxies)
    soup = BeautifulSoup(reqs.text, 'html.parser')
    json = html_to_json.convert(str(soup))
    true_json = jsn.loads(jsn.dumps(json))
    horse_powers = \
        true_json['html'][0]['body'][0]['main'][0]['div'][0]['div'][2]['div'][1]['div'][0]['div'][0]['div'][2]['div'][
            4]['div'][4]['ul'][0]['li'][0]['div'][0]['p'][0]['_value'].split(' ', 1)[0]
    price = \
    true_json['html'][0]['body'][0]['main'][0]['div'][0]['div'][2]['div'][1]['div'][0]['div'][0]['div'][0]['div'][1][
        'div'][1]['p'][0]['_values']
    min_price = price[1]
    max_price = price[3]
    return [horse_powers, str(min_price).replace(',', '.'), str(max_price).replace(',', '.')]
