import json
import os.path
import csv

grid = '/Users/myeong/git/DSSG/Project/milano-grid.geojson'

# this file is too big to include in Github
cdr = '/Users/myeong/git/DSSG/Project/sms-call-internet-mi-2013-11-01.txt' 

with open(grid) as f:
    new_grid = json.load(f)

def put_sms_in(cid, num):
    for cell in new_grid['features']:
        if cell['properties']['cellId'] == cid:            
            if 'sms_in' not in cell['properties']:
                cell['properties']['sms_in'] = float(num)
            else:
                cell['properties']['sms_in'] += float(num)
            break
    return

def put_sms_out(cid, num):
    for cell in new_grid['features']:
        if cell['properties']['cellId'] == cid:            
            if 'sms_out' not in cell['properties']:
                cell['properties']['sms_out'] = float(num)
            else:
                cell['properties']['sms_out'] += float(num)
            break
    return

def put_call_in(cid, num):
    for cell in new_grid['features']:
        if cell['properties']['cellId'] == cid:            
            if 'call_in' not in cell['properties']:
                cell['properties']['call_in'] = float(num)
            else:
                cell['properties']['call_in'] += float(num)
            break
    return

def put_call_out(cid, num):
    for cell in new_grid['features']:
        if cell['properties']['cellId'] == cid:            
            if 'call_out' not in cell['properties']:
                cell['properties']['call_out'] = float(num)
            else:
                cell['properties']['call_out'] += float(num)
            break
    return

def put_internet(cid, num):
    for cell in new_grid['features']:
        if cell['properties']['cellId'] == cid:            
            if 'internet' not in cell['properties']:
                cell['properties']['internet'] = float(num)
            else:
                cell['properties']['internet'] += float(num)
            break
    return

# this function put variables and aggregate numbers in GeoJSON file
def count_cdr():
    k = 0

    if os.path.isfile(cdr):
        with open(cdr) as data_file:
            data = csv.reader(data_file, delimiter="\t")

            for element in data:       
                k += 1 
                if element[3] != '':
                    put_sms_in(int(element[0]), element[3])
                if element[4] != '':
                    put_sms_out(int(element[0]), element[4])
                if element[5] != '':
                    put_call_in(int(element[0]), element[5])
                if element[6] != '':
                    put_call_out(int(element[0]), element[6])
                if element[7] != '':
                    put_internet(int(element[0]), element[7])

                if k%10000 == 0:
                    print (k)
            return new_grid

    # i = 0
    # for c in new_grid['features']:
    #     print (c)
    #     i += 1 
    #     if i == 10:
    #         break




