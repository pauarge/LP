from urllib.request import urlopen
import xml.etree.ElementTree as ET

import argparse
import sys

URL_BICING = 'http://wservice.viabicing.cat/v1/getstations.php?v=1'
URL_EVENTS = 'http://www.bcn.cat/tercerlloc/agenda_cultural.xml'
URL_PARKING = 'http://www.bcn.cat/tercerlloc/Aparcaments.xml'


def fetch_stations():
    data = urlopen(URL_BICING).read()
    root = ET.fromstring(data)
    return root.iter('station')


def fetch_events():
    data = urlopen(URL_EVENTS).read()
    root = ET.fromstring(data)
    return root.find('search').find('queryresponse').find('list').find('list_items').iter('row')


def fetch_parking():
    data = urlopen(URL_PARKING).read()
    root = ET.fromstring(data)
    return root.find('search').find('queryresponse').find('list').find('list_items').iter('row')


def fetch_data():
    fetch_stations()
    fetch_events()
    fetch_parking()


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--date')
    parser.add_argument('-k', '--key')

    args = parser.parse_args()

    fetch_data()


if __name__ == '__main__':
    main(sys.argv)
