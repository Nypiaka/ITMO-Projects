import csv
import pandas as pd

df_friends = pd.read_csv('processed/friends.csv')
left = df_friends['user1']
right = df_friends['user2']

map = {}

for i in range (0, len(left)):
    if (left[i] in map):
        map[left[i]].add(right[i])
    else:
        map[left[i]] = set()
        map[left[i]].add(right[i])

df_pairs = pd.read_csv('processed/links_test.csv')

users1 = df_pairs['user1']
users2 = df_pairs['user2']
f = open('processed/result.csv', 'w')
writer = csv.writer(f)
writer.writerow(['ID','is_friends'])
for i in range (0, len(users1)):
    set1 = map[users1[i]]
    set2 = map[users2[i]]
    intersected = set1.intersection(set2)
    if (len(intersected) >= 2):
        writer.writerow([i, 1])
    else:
        writer.writerow([i, 0])
