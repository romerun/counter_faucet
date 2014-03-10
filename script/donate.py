import json
import requests
from requests.auth import HTTPBasicAuth
from random import shuffle
import time

##################### config #######################

#give to beggars having the history of income less than or euqal to X satoshis
max_credit_satoshi=int(1e8)

#in the future we might be able to giveaway other assets than XCP
asset="XCP"

#donation source addresses, be sure to unlock your wallet for a long time (since bitcoind the change needs to be confirmed before sending a new one)
#if you have many of them it will speed things up
donation_addresses=['1HGT1utMx3JbkDyrCiH3rf84FzK1BVEhSm','1NhGMimWGD37EDVVQiy8xfjXer72ywFTHB','13LVzzK1wEGH51gyn3BPga2wtaYf7v3eWP']

faucet_url = "http://xcp.bfolder.com/api/hungry_beggars?asset=%s&amount=%d&include_id=false" % (asset,max_credit_satoshi)

#some say it needs at least this satoshis, not sure, not much important really
minimum_btc_on_address = 31720

#in satoshis of course
giveaway_per_beggar= int(1e7)
#

#### your counterparty config
counterparty_url = "http://localhost:4000/api/"
headers = {'content-type': 'application/json'}
auth = HTTPBasicAuth('rpcuser', 'graffitiseis')
#

###################################################################################################

#note.., might need to handle bignum properly, no clue to do in python, leave it for excercise

#stolen from counterpartyd
def get_btc_balance(address):
    r = requests.get("https://blockchain.info/q/addressbalance/" + address + "?confirmations=1")
    # ^any other services that provide this?? (blockexplorer.com doesn't...)
    try:
        assert r.status_code == 200
        return int(r.text) / float(config.UNIT)
    except:
        return -1

def get_ready_address():
    for donation_address in donation_addresses:
        if int(get_btc_balance (donation_address)) >= minimum_btc_on_address:
            return donation_address
    
    print("no donation addresses available")
    time.sleep(5*60)
    get_ready_address()

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
        total = total + int(credit['amount'])

    if total > max_credit_satoshi:
        return False
    else:
        return True
    
response = requests.get(faucet_url)
beggars = response.json()
shuffle(beggars)

for beggar in beggars:
    source_address = get_ready_address()

    print ("%s" % beggar['username'])
    if check_credit (beggar['address']):
        payload = {
            "method": "create_send",
            "params": [source_address, beggar['address'], "XCP", giveaway_per_beggar],
            "jsonrpc": "2.0",
            "id": 0,
        }

        response = requests.post(counterparty_url, data=json.dumps(payload), headers=headers, auth=auth)
        response = response.json()
        tx_hex = response['result']

        payload = {
            "method": "transmit",
            "params": [tx_hex],
            "jsonrpc": "2.0",
            "id": 0,
        }

        response = requests.post(counterparty_url, data=json.dumps(payload), headers=headers, auth=auth)
        response = response.json()
        tx_id = response['result']

        print("sent from %s to %s: response = %s\n" % (source_address, beggar['username'], tx_id))
        time.sleep(1)
    else:
        print("skip this rich beggar %s\n" % (beggar['username']))

print("\nAll sent\n");
