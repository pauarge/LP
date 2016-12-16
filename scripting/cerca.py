import xml.etree.ElementTree as ET


import urllib
import argparse
import sys


URL_BICING = 'http://wservice.viabicing.cat/v1/getstations.php?v=1'
URL_EVENTS = 'http://w10.bcn.es/APPS/asiasiacache/peticioXmlAsia?id=203'
URL_PARKING = 'http://www.bcn.cat/tercerlloc/Aparcaments.xml'


def fetch_data():
    tree = ET.parse('country_data.xml')
    root = tree.getroot(URL_BICING)
    file = urllib.urlopen(
        'https://www.goodreads.com/review/list/20990068.xml?key=nGvCqaQ6tn9w4HNpW8kquw&v=2&shelf=toread')
    data = file.read()
    file.close()

    print("Got data")


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--date')
    parser.add_argument('-k', '--key')

    args = parser.parse_args()

    fetch_data()


if __name__ == '__main__':
    main(sys.argv)
