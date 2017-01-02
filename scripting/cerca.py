from urllib.request import urlopen
from datetime import datetime, timedelta
from math import radians, cos, sin, asin, sqrt
from typing import Generator

import xml.etree.ElementTree as ET
import argparse
import ast
import os
import unicodedata

URL_BICING = 'http://wservice.viabicing.cat/v1/getstations.php?v=1'
URL_EVENTS = 'http://www.bcn.cat/tercerlloc/agenda_cultural.xml'
URL_PARKING = 'http://www.bcn.cat/tercerlloc/Aparcaments.xml'
DIST_DECIMAL_PLACES = 3


# Returns straight line distance between two geographical points in km
def distance(lon1: float, lat1: float, lon2: float, lat2: float) -> float:
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    a = sin(dlat / 2) ** 2 + cos(lat1) * cos(lat2) * sin(dlon / 2) ** 2
    c = 2 * asin(sqrt(a))
    return 6367 * c


def remove_accents(input_str: str) -> str:
    nfkd_form = unicodedata.normalize('NFKD', input_str.casefold())
    return u"".join([c for c in nfkd_form if not unicodedata.combining(c)])


class Event(object):
    def __init__(self, name: str, address: str, district: str, timestamp: str, lat: str, lon: str):
        self.name = name
        self.address = address
        self.district = district
        self.timestamp = datetime.strptime(timestamp, "%d/%m/%Y%H:%M")
        self.lat = float(lat)
        self.lon = float(lon)

    @classmethod
    def filter_key(cls, events: list, filters: list) -> list:
        if len(filters) > 0:
            if isinstance(filters[0], list):
                return cls.filter_key(cls.filter_key(events, filters[1:]), filters[0])
            elif isinstance(filters[0], tuple):
                return filter(lambda x: any(
                    remove_accents(y) in remove_accents(x.name) or
                    remove_accents(y) in remove_accents(x.address) or
                    remove_accents(y) in remove_accents(x.district)
                    for y in filters[0]),
                              cls.filter_key(events, filters[1:]))
            elif isinstance(filters[0], str):
                return filter(lambda x:
                              remove_accents(filters[0]) in remove_accents(x.name) or
                              remove_accents(filters[0]) in remove_accents(x.address) or
                              remove_accents(filters[0]) in remove_accents(x.district),
                              cls.filter_key(events, filters[1:]))
        return events

    @classmethod
    def get_timestamps(cls, filters: list) -> list:
        if isinstance(filters, list) and len(filters) > 0:
            return cls.get_timestamps(filters[0]) + cls.get_timestamps(filters[1:])
        elif isinstance(filters, tuple):
            base = datetime.strptime(filters[0], "%d/%m/%Y")
            return [(base + timedelta(days=filters[1]), base + timedelta(days=1 + filters[2]))]
        elif isinstance(filters, str):
            base = datetime.strptime(filters, "%d/%m/%Y")
            return [(base, base + timedelta(days=1))]
        return []

    @classmethod
    def filter_date(cls, events: list, filters: list) -> list:
        if len(filters) > 0:
            return filter(lambda x: any(y[0] <= x.timestamp <= y[1] for y in cls.get_timestamps(filters)), events)
        return events

    @classmethod
    def fetch(cls, args: list) -> list:
        data = urlopen(URL_EVENTS).read()
        rows = ET.fromstring(data).find('search').find('queryresponse').find('list').find('list_items').iter('row')
        events = []
        errs = 0
        for i in rows:
            try:
                item = i.find('item')
                addresses = item.find('addresses').find('item')
                event = Event(item.find('name').text, addresses.find('address').text, addresses.find('district').text,
                              item.find('proxdate').text + item.find('proxhour').text,
                              addresses.find('gmapx').text, addresses.find('gmapy').text)
                events.append(event)
            except AttributeError:
                errs += 1

        filters_key = ast.literal_eval(args.key) if args.key else []
        filters_date = ast.literal_eval(args.date) if args.date else []

        events = cls.filter_key(events, filters_key)
        events = cls.filter_date(events, filters_date)
        return sorted(events, key=lambda x: x.timestamp)


class Station(object):
    def __init__(self, slots: str, bikes: str, street: str, number: str, lat: str, lon: str):
        self.slots = int(slots)
        self.bikes = int(bikes)
        self.street = street
        self.number = number
        self.lat = float(lat)
        self.lon = float(lon)
        self.distance = 0

    @staticmethod
    def fetch() -> list:
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
    def __init__(self, name: str, street: str, lat: str, lon: str):
        self.name = name
        self.street = street
        self.lat = float(lat)
        self.lon = float(lon)
        self.distance = 0

    @staticmethod
    def fetch() -> list:
        data = urlopen(URL_PARKING).read()
        rows = ET.fromstring(data).find('search').find('queryresponse').find('list').find('list_items').iter('row')
        parkings = []
        errs = 0
        for i in rows:
            try:
                item = i.find('item')
                addresses = item.find('addresses').find('item')
                parking = Parking(item.find('name').text, addresses.find('address').text, addresses.find('gmapx').text,
                                  addresses.find('gmapy').text)
                parkings.append(parking)
            except AttributeError:
                errs += 1
        return parkings


class Printable(object):
    def __init__(self, event: Event, stations: list, parkings: list):
        self.event = event
        self.global_stations = stations
        self.global_parkings = parkings

        self.sorted_stations = self.sort_stations()
        self.stations_slots = self.fetch_stations_slots()
        self.stations_bikes = self.fetch_stations_bikes()
        self.parkings = self.fetch_parkings()

    def sort_stations(self) -> list:
        for i in self.global_stations:
            i.distance = distance(self.event.lon, self.event.lat, i.lon, i.lat)
        stations = list(filter(lambda x: x.distance <= 0.5, self.global_stations))
        return sorted(stations, key=lambda x: x.distance)

    def fetch_stations_slots(self) -> list:
        return list(filter(lambda x: x.slots > 0, self.sorted_stations))

    def fetch_stations_bikes(self) -> list:
        return list(filter(lambda x: x.bikes > 0, self.sorted_stations))

    def fetch_parkings(self) -> list:
        for i in self.global_parkings:
            i.distance = distance(self.event.lon, self.event.lat, i.lon, i.lat)
        return sorted(list(filter(lambda x: x.distance <= 0.5, self.global_parkings)), key=lambda x: x.distance)

    @staticmethod
    def generate_html(events: list, stations: list, parkings: list) -> Generator:
        yield '<html>' \
              '<head>' \
              '<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">' \
              '<meta charset="UTF-8">' \
              '</head>' \
              '<body>' \
              '<div class="container">' \
              '<h1>Results:</h1><br>'
        if events:
            for e in events:
                p = Printable(e, stations, parkings)
                yield '<h2>{}</h2>'.format(p.event.name)
                yield '<p>{} - {}</p>'.format(p.event.address, datetime.strftime(p.event.timestamp, "%d/%m/%Y %H:%M"))
                if p.stations_slots:
                    yield '<h3>Stations with available slots</h3>' \
                          '<table class="table table-bordered table-hover">' \
                          '<tr>' \
                          '<th>Street</th><th>Number</th><th>Free slots</th><th>Available bikes</th><th>Distance</th>' \
                          '</tr>'
                    for i in p.stations_slots:
                        yield '<tr>' \
                              '<td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{} km</td>' \
                              '</tr>'.format(i.street, i.number, i.slots, i.bikes,
                                             round(i.distance, DIST_DECIMAL_PLACES))
                    yield '</table>'
                if p.stations_bikes:
                    yield '<h3>Stations with available bikes</h3>' \
                          '<table class="table table-bordered table-hover">' \
                          '<tr>' \
                          '<th>Street</th><th>Number</th><th>Free slots</th><th>Available bikes</th><th>Distance</th>' \
                          '</tr>'
                    for i in p.stations_bikes:
                        yield '<tr>' \
                              '<td>{}</td><td>{}</td><td>{}</td><td>{}</td><td>{} km</td>' \
                              '</tr>'.format(i.street, i.number, i.slots, i.bikes,
                                             round(i.distance, DIST_DECIMAL_PLACES))
                    yield '</table>'
                if p.parkings:
                    yield '<h3>Nearby parking lots</h3>' \
                          '<table class="table table-bordered table-hover">' \
                          '<tr><th>Name</th><th>Address</th><th>Distance</tr>'
                    for i in p.parkings:
                        yield '<tr>' \
                              '<td>{}</td><td>{}</td><td>{} km</td>' \
                              '</tr>'.format(i.name, i.street, round(i.distance, DIST_DECIMAL_PLACES))
                    yield '</table>'
                yield '<hr>'
        else:
            yield 'No results.'
        yield '</div></body></html>'


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d', '--date')
    parser.add_argument('-k', '--key')
    args = parser.parse_args()

    print('Getting events...')
    events = Event.fetch(args)

    print('Getting stations...')
    stations = Station.fetch()

    print('Getting parking slots...')
    parkings = Parking.fetch()

    print('Generating HTML...')
    html = '\n'.join(Printable.generate_html(events, stations, parkings))

    print('Saving to disk...')
    if not os.path.exists(os.path.dirname("out/")):
        os.makedirs(os.path.dirname("out/"))
    with open("out/{}.html".format(datetime.now()), "w") as out_file:
        print(html, file=out_file)


if __name__ == '__main__':
    main()
