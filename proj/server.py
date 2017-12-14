import asyncio
from time import time
import json
import sys

server_id = None
clientList = {}     # [name] = (name, x, y, time)
serverList = {'Alford': 8880,
              'Ball': 8882,
              'Hamilton': 8884,
              'Holiday': 8886,
              'Welsh': 8888}

serverConnections = {'Alford': ('Hamilton', 'Welsh'),
                     'Ball': ('Holiday', 'Welsh'),
                     'Hamilton': ('Alford', 'Holiday'),
                     'Holiday': ('Ball', 'Hamilton'),
                     'Welsh': ('Alford', 'Ball')}

at_list = {}

loop = None

class EchoServerClientProtocol(asyncio.Protocol):
    def connection_made(self, transport):
        peername = transport.get_extra_info('peername')
        # print('Connection from {}'.format(peername))
        self.transport = transport

    @asyncio.coroutine
    def propagator(self, message, port):
        global loop

        reader = None
        writer = None

        try:
            reader, writer = yield from asyncio.open_connection('127.0.0.1', port)
        except:
            print("Port {} closed.".format(port))
            return

        writer.write(message.encode())

        writer.close()

    def propagate(self, msg):
        global loop

        for conn in serverConnections[server_id]:
            port = serverList[conn]
            asyncio.ensure_future(self.propagator(msg, port))

    def write_at(self, cli_name, prop):
        client = clientList[cli_name]
        time_diff = str(time() - float(client[3]))

        if (time_diff[0] != '-'):
            time_diff = '+' + time_diff;

        at = "AT {} {} {} {}{} {}".format(server_id, time_diff, client[0], client[1], client[2], client[3])
        self.transport.write(str.encode(at))

        if prop:
            at_list[client[0]] = at
            self.propagate(at)

    def handle_iamat(self, msg, raw_str):
        # iamat information
        cli_name = msg[1]
        location = msg[2].replace("+", " +").replace("-", " -").split(' ')

        if ((msg[2].count('+') + msg[2].count('-')) != 2 or len(location) != 3):
            self.transport.write(str.encode('? ' + raw_str))
            self.transport.close()
            return

        loc_x = location[1]
        loc_y = location[2]

        if (loc_x.find('.') == -1 or loc_y.find('.') == -1):
            self.transport.write(str.encode('? ' + raw_str))
            self.transport.close()
            return

        if (len(loc_x) - loc_x.index('.') < 5 or len(loc_y) - loc_y.index('.') < 5):
            self.transport.write(str.encode('? ' + raw_str))
            self.transport.close()
            return

        cli_time = msg[3].replace("\r\n", "")
        if (float(cli_time) < 0):
            self.transport.write(str.encode('? ' + raw_str))
            self.transport.close()
            return


        late_cli_time = cli_time

        if (clientList.get(cli_name) == None):
            clientList[cli_name] = (cli_name, loc_x, loc_y, cli_time)
            self.write_at(cli_name, True)
        elif (cli_time <= clientList[cli_name][3]):
            late_cli_time = clientList[cli_name][3]
            clientList[cli_name] = (cli_name, loc_x, loc_y, cli_time)
            self.transport.write(at_list[cli_name].encode())
            # self.write_at(cli_name, False)
        else:
            clientList[cli_name] = (cli_name, loc_x, loc_y, cli_time)
            self.write_at(cli_name, True)

        clientList[cli_name] = (cli_name, loc_x, loc_y, late_cli_time)
        self.transport.close()

    def handle_at(self, msg, raw_str):
        cli_name = msg[3]
        location = msg[4].replace("+", " +").replace("-", " -").split(' ')
        loc_x = location[1]
        loc_y = location[2]
        cli_time = msg[5].replace("\r\n", "")

        if (at_list.get(msg[3]) == None or clientList.get(cli_name) == None or clientList[cli_name][3] < cli_time):
            clientList[cli_name] = (cli_name, loc_x, loc_y, cli_time)
            at_list[msg[3]] = raw_str
            self.propagate(raw_str)




    @asyncio.coroutine
    def handle_GET_request(self, msg):
        # CREATING GET requests

        # https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=-33.8670522,151.1957362&
        # radius=500&types=food&name=cruise&key=API_KEY

        cli_name = msg[1]
        radius = int(msg[2])
        info = int(msg[3])


        client = clientList.get(cli_name)
        if (client == None):
            print("ID does not exist in database.")
            self.transport.close()
            return


        location = "{},{}".format(client[1],client[2]).replace("+", "")

        key = 'AIzaSyBw0XHL2oIY2QI-BIFd76xoiSzOLUa21VM'
        uri = "/maps/api/place/nearbysearch/json?location={}&radius={}&key={}".format(location, radius, key)

        GET  = "GET {} HTTP/1.1\r\n".format(uri)
        GET += "Host:maps.googleapis.com\r\n"
        GET += "Content-Type: text/plain; charset=utf-8\r\n"
        GET += "\r\n\r\n"

        connect = asyncio.open_connection('maps.googleapis.com', 443, ssl=True)
        reader, writer = yield from connect

        writer.write(GET.encode('utf-8'))

        output = ""

        while True:
            line = yield from reader.readline()
            line = line.decode('utf-8')
            if line[0] == '0':
                break
            output += line

        parsed_output = output.split('\r\n\r\n')
        json_file = parsed_output[1]

        json_file = json_file[json_file.find('{'):]

        json_output = json.loads(json_file)
        json_output["results"] = json_output["results"][:info]

        self.transport.write(str.encode(at_list[cli_name]))
        self.transport.write(str.encode(json.dumps(json_output, indent=3)))
        self.transport.write(str.encode('\n'))

        writer.close()
        self.transport.close()


    def handle_whatsat(self, msg, raw):
        cli_name = msg[1]
        try:
            radius = int(msg[2])
            if (radius > 50 or radius < 0):
                self.transport.write(str.encode("? " + raw))
                self.transport.close()
                return
        except:
            self.transport.write(str.encode("? " + raw))
            self.transport.close()
            return

        try:
            info = int(msg[3])
            if (info > 20 or info < 0):
                self.transport.write(str.encode("? " + raw))
                self.transport.close()
                return
        except:
            self.transport.write(str.encode("? " + raw))
            self.transport.close()
            return

        asyncio.ensure_future(self.handle_GET_request(msg))

    def parse_message(self, message, raw):
        parsed_msg = message.split(' ')

        if (parsed_msg[0] == 'IAMAT' and len(parsed_msg) == 4):
            self.handle_iamat(parsed_msg, message)
        elif (parsed_msg[0] == 'WHATSAT' and len(parsed_msg) == 4):
            self.handle_whatsat(parsed_msg, message)
        elif (parsed_msg[0] == 'AT'):
            self.handle_at(parsed_msg, message)
        else:
            self.transport.write(str.encode("? {}".format(message)))
            self.transport.close()

    def data_received(self, data):
        message = data.decode()
        print(message)
        self.parse_message(message, data)



def main():
    global server_id, loop

    # Argument Handling
    if (len(sys.argv) != 2):
        print("Incorrect Number of Arguments.")
        exit(1)

    server_id = sys.argv[1]
    port = serverList.get(server_id)

    if (port == None):
        print("Incorrect Server Name.")
        exit(1)

    # Starting Server
    loop = asyncio.get_event_loop()
    # Each client connection will create a new protocol instance
    coro = loop.create_server(EchoServerClientProtocol, '127.0.0.1', port)
    server = loop.run_until_complete(coro)

    # Serve requests until Ctrl+C is pressed
    # print('Serving on {}'.format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    print (clientList['kiwi.cs.ucla.edu'])

    # Close the server
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()

if __name__ == '__main__':
    main()
