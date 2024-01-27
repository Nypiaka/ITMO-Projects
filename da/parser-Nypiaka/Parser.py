import random
import concurrent.futures as mt

import numpy as np
import pandas as pd
import requests
import html_to_json
import json
from bs4 import BeautifulSoup

from PageParser import parsePage

LIMIT_TO_FOUND = 3000

headers = {
    'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:109.0) Gecko/20100101 Firefox/117.0',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.5',
    'Connection': 'keep-alive',
    'Upgrade-Insecure-Requests': '1',
    'Sec-Fetch-Dest': 'document',
    'Sec-Fetch-Mode': 'navigate',
    'Sec-Fetch-Site': 'none',
    'Sec-Fetch-User': '?1',
}

proxies_list = ['85.208.98.23', '216.158.234.82', '172.241.24.83', '35.205.72.231', '52.67.204.189', '192.248.181.140',
                '18.233.58.241', '146.59.0.13']

proxies = {
    'http': random.choice(proxies_list)
}

TEMPLATE_LINK = "https://cars.usnews.com/cars-trucks/browse?sort=Alphabetical&page=6"
main_page = requests.get(TEMPLATE_LINK, headers=headers)
soup = BeautifulSoup(main_page.text, 'html.parser')
string_json = html_to_json.convert(str(soup))
true_json = json.loads(json.dumps(string_json))
half_way_str = true_json['html'][0]['body'][0]['script'][0]['_value']
true_json = half_way_str[half_way_str.find('{', half_way_str.find('{') + 1):len(half_way_str) - 1]
true_json = json.loads(true_json.replace('undefined', '"undefined"'))
all_cars = true_json['src/containers/pages/autos/carfinder/index.js']['data']['data']['autos_meta'][
    'global_mmy_choices_v2']

executor = mt.ThreadPoolExecutor(max_workers=16)
results = []
for i in range(len(all_cars)):
    def task():
        label = all_cars[i]['make']
        for lineup in all_cars[i]['models']:
            model = lineup['model']
            for year in lineup['years']:
                if len(results) < LIMIT_TO_FOUND:
                    try:
                        parsed = parsePage('https://cars.usnews.com/' + year['url'], proxies)
                        results.append([label, model, year['year']] + parsed)
                    except IndexError:
                        pass


    executor.submit(task)
while True:
    if len(results) >= LIMIT_TO_FOUND:
        executor.shutdown()
        break

pd.DataFrame(np.array(results)).to_csv('data.csv', header=['Label', 'Model', 'Year', 'HP', 'Min cost', 'Max cost'])
