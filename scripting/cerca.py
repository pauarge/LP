# TODO: Parallelize data download and parsing

from urllib.request import urlopen
import xml.etree.ElementTree as ET
from math import radians, cos, sin, asin, sqrt

import argparse
import sys

URL_BICING = 'http://wservice.viabicing.cat/v1/getstations.php?v=1'
URL_EVENTS = 'http://www.bcn.cat/tercerlloc/agenda_cultural.xml'
URL_PARKING = 'http://www.bcn.cat/tercerlloc/Aparcaments.xml'


class Event(object):
    def __init__(self, name, address, timestamp, lat, lon):
        self.name = name
        self.address = address
        self.timestamp = timestamp
        self.lat = lat
        self.lon = lon

    @staticmethod
    def fetch():
        print("Getting events...")
        data = urlopen(URL_EVENTS).read()
        root = ET.fromstring(data)
        rows = root.find('search').find('queryresponse').find('list').find('list_items').iter('row')
        events = []
        for i in rows:
            item = i.find('item')
            addresses = item.find('addresses')
            event = Event(item.find('name').text, addresses.find('address').text,
                          item.find('proxdate').text + item.find('proxhour').text, addresses.find('gmapx'),
                          addresses.find('gmapy'))
            events.append(event)
        return events


class Station(object):
    def __init__(self, slots, bikes, street, number, lat, lon):
        self.slots = slots
        self.bikes = bikes
        self.street = street
        self.number = number
        self.lat = lat
        self.lon = lon

    @staticmethod
    def fetch():
        print("Getting stations...")
        data = urlopen(URL_BICING).read()
        root = ET.fromstring(data)
        return root.iter('station')


class Parking(object):
    def __init__(self, name, lat, lon):
        self.name = name
        self.lat = lat
        self.lon = lon

    @staticmethod
    def fetch():
        print("Getting parkings...")
        data = urlopen(URL_PARKING).read()
        root = ET.fromstring(data)
        return root.find('search').find('queryresponse').find('list').find('list_items').iter('row')


class Printable(object):
    def __init__(self, event, stations, parkings):
        self.event = event
        self.stations = stations
        self.parkings = parkings

    @staticmethod
    def generate_html(printables):
        print("Exporting data...")
        yield '<table>'
        for event in printables:
            yield '  <tr>'
            yield '    <td>Hola</td>'
            yield '  </tr>'
        yield '</table>'


def distance(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat / 2) ** 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ** 2
    c = 2 * asin(sqrt(a))
    km = 6367 * c
    return km


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--date')
    parser.add_argument('-k', '--key')
    args = parser.parse_args()

    events = Event.fetch()
    stations = Station.fetch()
    parkings = Parking.fetch()

    printables = [Printable(x, stations, parkings) for x in events]
    html = '\n'.join(Printable.generate_html(printables))

    with open("out.html", "w") as out_file:
        print(html, file=out_file)


if __name__ == '__main__':
    main(sys.argv)
