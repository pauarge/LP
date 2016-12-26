# TODO: Parallelize data download and parsing
#       Add street number to event

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
        self.number = None
        self.timestamp = datetime.strptime(timestamp, "%d/%m/%Y%H:%M")
        self.lat = float(lat)
        self.lon = float(lon)

    @staticmethod
    def fetch(args):
        print("Getting events...")
        data = urlopen(URL_EVENTS).read()
        rows = ET.fromstring(data).find('search').find('queryresponse').find('list').find('list_items').iter('row')
        events = []
        errs = 0
        for i in rows:
            try:
                item = i.find('item')
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
        self.slots = int(slots)
        self.bikes = int(bikes)
        self.street = street
        self.number = number
        self.lat = float(lat)
        self.lon = float(lon)
        self.distance = 0

    @staticmethod
    def fetch():
        print("Getting stations...")
        data = urlopen(URL_BICING).read()
        stations = []
        items = ET.fromstring(data).iter('station')
        errs = 0
        for item in items:
            try:
                station = Station(item.find('slots').text, item.find('bikes').text, item.find('street').text,
                                  item.find('streetNumber').text, item.find('lat').text, item.find('long').text)
                stations.append(station)
            except AttributeError:
                errs += 1
        return stations


class Parking(object):
    def __init__(self, name, street, number, lat, lon):
        self.name = name
        self.street = street
        self.number = number
        self.lat = float(lat)
        self.lon = float(lon)
        self.distance = 0

    @staticmethod
    def fetch():
        print("Getting parkings...")
        data = urlopen(URL_PARKING).read()
        rows = ET.fromstring(data).find('search').find('queryresponse').find('list').find('list_items').iter('row')
        parkings = []
        errs = 0
        for i in rows:
            try:
                item = i.find('item')
                addresses = item.find('addresses').find('item')
                parking = Parking(item.find('name').text, addresses.find('address').text,
                                  addresses.find('streetnum').text, addresses.find('gmapx').text,
                                  addresses.find('gmapy').text)
                parkings.append(parking)
            except AttributeError:
                errs += 1
        return parkings


class Printable(object):
    def __init__(self, event, stations, parkings):
        self.event = event
        self.global_stations = stations
        self.global_parkings = parkings

        self.sorted_stations = self.sort_stations()
        self.stations_spots = self.fetch_stations_spots()
        self.stations_bikes = self.fetch_stations_bikes()
        self.parkings = self.fetch_parkings()

    def sort_stations(self):
        for i in self.global_stations:
            i.distance = distance(self.event.lon, self.event.lat, i.lon, i.lat)
        return sorted(self.global_stations, key=lambda x: x.distance)

    def fetch_stations_spots(self):
        return []

    def fetch_stations_bikes(self):
        return []

    def fetch_parkings(self):
        return []

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
