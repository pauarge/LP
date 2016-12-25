# TODO: Parallelize data download and parsing

from urllib.request import urlopen
from datetime import datetime
from math import radians, cos, sin, asin, sqrt

import xml.etree.ElementTree as ET
import argparse
import sys

URL_BICING = 'http://wservice.viabicing.cat/v1/getstations.php?v=1'
URL_EVENTS = 'http://www.bcn.cat/tercerlloc/agenda_cultural.xml'
URL_PARKING = 'http://www.bcn.cat/tercerlloc/Aparcaments.xml'


class Event(object):
    def __init__(self, name, address, timestamp, lat, lon):
        self.name = name
        self.address = address
        self.timestamp = datetime.strptime(timestamp, "%d/%m/%Y%H:%M")
        self.lat = lat
        self.lon = lon

    @staticmethod
    def fetch(args):
        print("Getting events...")
        data = urlopen(URL_EVENTS).read()
        root = ET.fromstring(data)
        rows = root.find('search').find('queryresponse').find('list').find('list_items').iter('row')
        events = []
        errs = 0
        for i in rows:
            item = i.find('item')
            try:
                addresses = item.find('addresses').find('item')
                event = Event(item.find('name').text, addresses.find('address').text,
                              item.find('proxdate').text + item.find('proxhour').text,
                              addresses.find('gmapx').text, addresses.find('gmapy').text)
                events.append(event)
            except AttributeError:
                errs += 1
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
    def __init__(self, name, street, number, lat, lon):
        self.name = name
        self.street = street
        self.number = number
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
        self.global_stations = stations
        self.global_parkings = parkings

        self.stations_spots = []
        self.stations_bikes = []
        self.parkings = []

    @staticmethod
    def generate_html(printables):
        print("Exporting data...")
        yield '<html><head><meta charset="UTF-8"></head><body>'
        yield '<h1>Results</h1>'
        for p in printables:
            yield '<h2>{}</h2>'.format(p.event.name)
            yield '<p>{} - {}</p>'.format(p.event.address, p.event.timestamp)
            if p.stations_spots:
                yield '<h3>Stations with available spots</h3>'
            if p.stations_bikes:
                yield '<h3>Stations with available bikes</h3>'
            if p.parkings:
                yield '<h3>Nearby parking lots</h3>'
        yield '</body></html>'


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

    events = Event.fetch(args)
    stations = Station.fetch()
    parkings = Parking.fetch()

    printables = [Printable(x, stations, parkings) for x in events]
    html = '\n'.join(Printable.generate_html(printables))

    with open("out.html", "w") as out_file:
        print(html, file=out_file)


if __name__ == '__main__':
    main(sys.argv)
