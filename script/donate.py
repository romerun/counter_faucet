import json
import requests
from requests.auth import HTTPBasicAuth
from random import shuffle
import time
import sys

##################### user config #######################

#donation source addresses, be sure to unlock your wallet for a long time (since bitcoind the change needs to be confirmed before sending a new one)
#if you have many of them it will speed things up
donation_addresses=['1HGT1utMx3JbkDyrCiH3rf84FzK1BVEhSm','1NhGMimWGD37EDVVQiy8xfjXer72ywFTHB','13LVzzK1wEGH51gyn3BPga2wtaYf7v3eWP']

# give to beggars having the history of income less than or euqal to X satoshis
# 1e8 means that if an address has been given more than 1 XCP, we fill just filter it out of the list
max_credit_satoshi=int(1e8)

# in satoshis of course
# 1e7 means that we are to give 0.01 XCP per person
giveaway_per_beggar= int(2e7)
#

#in the future we might be able to giveaway other assets than XCP
asset="XCP"

#### your counterparty config
counterparty_url = "http://localhost:4000/api/"
headers = {'content-type': 'application/json'}
auth = HTTPBasicAuth('rpcuser', 'graffitiseis')
#

###################################################################################################

#note.., might need to handle bignum properly, no clue to do in python, leave it for excercise

def satoshi_of_float(f):
    return 1e8 * f

def float_of_satoshi(sato):
    return sato / 1e8

def try_to_send(destination,index):
    if index > len(donation_addresses)-1:
        print("no donation addresses available, wait 5 minutes, make sure that you unlock the wallet and have enough BTC/XCP")
        time.sleep(5*60)
        try_to_send(destination,0)
    else:
        payload = {
            "method": "create_send",
            "params": [donation_addresses[index], destination, "XCP", giveaway_per_beggar],
            "jsonrpc": "2.0",
            "id": 0,
        }

        response = requests.post(counterparty_url, data=json.dumps(payload), headers=headers, auth=auth)
        response = response.json()
        if 'result' in response:
            tx_hex = response['result']

            payload = {
                "method": "sign_tx",
                "params": [tx_hex],
                "jsonrpc": "2.0",
                "id": 0,
            }

            response = requests.post(counterparty_url, data=json.dumps(payload), headers=headers, auth=auth)
            response = response.json()

            if 'result' in response:
                signed_tx = response['result']

                payload = {
                    "method": "broadcast_tx",
                    "params": [signed_tx],
                    "jsonrpc": "2.0",
                    "id": 0,
                }

                response = requests.post(counterparty_url, data=json.dumps(payload), headers=headers, auth=auth)
                response = response.json()

                if 'result' in response:
                    tx_id = response['result']
                    print("sent from %s to %s: response = %s\n" % (donation_addresses[index], destination, tx_id))
                    time.sleep(1)
                else:
                    try_to_send(destination,index+1)
            else:
                try_to_send(destination,index+1)
        else:
            #print("address(%s) not available - %s" % (donation_addresses[index],response))
            try_to_send(destination,index+1)

def check_credit(address):
    payload = {
        "method": "get_credits",
        "params": {'filters':[{'field':'address','op': '==', 'value': address}, {'field':'asset','op': '==', 'value': asset}],"filterop": 'and'},
        "jsonrpc": "2.0",
        "id": 0
    }

    response = requests.post(counterparty_url, data=json.dumps(payload), headers=headers, auth=auth)
    credits = response.json()

    total = 0
    for credit in credits['result']:
        total = total + int(credit['quantity'])

    if total > max_credit_satoshi:
        return False
    else:
        return True


sys.stdout.write ("Get address whose credit is less than [%s]: " % (float_of_satoshi(max_credit_satoshi)))
sys.stdout.flush()
line = sys.stdin.readline()
line = line.rstrip()
if line != '':
    max_credit_satoshi = satoshi_of_float(float(line))

faucet_url = "http://xcp.bfolder.com/api/hungry_beggars?asset=%s&amount=%d&include_id=false" % (asset,max_credit_satoshi)

response = requests.get(faucet_url)
beggars = response.json()

beggars = [i for i in beggars if ((not ('sockpuppet' in i)) or (not i['sockpuppet']))]

print (beggars)
print("")
for beggar in beggars:
    print ("%s %s = %s %s" % (beggar['username'],beggar['address'],float_of_satoshi(float(beggar['amount'])),beggar['asset']))
print("")

sys.stdout.write ("Give [%s] per person: " % (float_of_satoshi(giveaway_per_beggar)))
sys.stdout.flush()
line = sys.stdin.readline()
line = line.rstrip()
if line != '':
    giveaway_per_beggar = satoshi_of_float(float(line))

shuffle(beggars)

for beggar in beggars:
    print ("%s" % beggar['username'])
    if check_credit (beggar['address']):
        try_to_send(beggar['address'],0)
    else:
        print("skip this rich beggar %s\n" % (beggar['username']))

print("\nAll sent\n");
