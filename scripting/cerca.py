from urllib.request import urlopen
import xml.etree.ElementTree as ET

import argparse
import sys

URL_BICING = 'http://wservice.viabicing.cat/v1/getstations.php?v=1'
URL_EVENTS = 'http://www.bcn.cat/tercerlloc/agenda cultural.xml'
URL_PARKING = 'http://www.bcn.cat/tercerlloc/Aparcaments.xml'


def fetch_stations():
    pass


def fetch_events():
    pass


def fetch_parking():
    pass


def fetch_data():
    data = urlopen(URL_BICING).read()

    root = ET.fromstring(data)
    for child in root.iter('station'):
        print(child.tag)


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--date')
    parser.add_argument('-k', '--key')

    args = parser.parse_args()

    fetch_data()


if __name__ == '__main__':
    main(sys.argv)
