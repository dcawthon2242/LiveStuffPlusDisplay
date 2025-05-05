import requests
import time
from gpiozero import Button

# --- GPIO Switch Setup ---
switch = Button(14)  # connected to GPIO14

def get_batter_side():
    return "Left" if switch.is_pressed else "Right"

# --- TrackMan Auth ---
def get_access_token(client_id, client_secret):
    url = "https://identity.trackmanbaseball.com/connect/token"
    data = {
        "grant_type": "client_credentials",
        "client_id": client_id,
        "client_secret": client_secret,
        "scope": "tmapi"
    }
    res = requests.post(url, data=data)
    return res.json().get("access_token")

# --- Get Most Recent Session ---
def get_latest_session(headers):
    res = requests.post("https://dataapi.trackmanbaseball.com/api/v1/discovery/practice/sessions", headers=headers)
    return res.json()[0]["sessionId"]

# --- Poll New Pitches ---
def get_pitches(headers, session_id):
    url = f"https://dataapi.trackmanbaseball.com/api/v1/data/practice/balls/{session_id}"
    return requests.get(url, headers=headers).json()

# --- Simulated Stuff+ Model (replace with real model call) ---
def calculate_stuff_plus(pitch_data, batter_side):
    # Placeholder: add your R or Python model call here
    print(f"Calculating Stuff+ for {batter_side}-handed batter with pitch:", pitch_data)
    return 110.5  # Dummy value

# --- Main Program ---
client_id = "CalStateFullerton-StuffPlus"
client_secret = "YOUR_SECRET_HERE"  # put in .env or keep secure

token = get_access_token(client_id, client_secret)
headers = {"Authorization": f"Bearer {token}"}
session_id = get_latest_session(headers)
print("Tracking session:", session_id)

seen_ids = set()

while True:
    pitches = get_pitches(headers, session_id)
    for pitch in pitches:
        pid = pitch["id"]
        if pid not in seen_ids:
            seen_ids.add(pid)
            batter_side = get_batter_side()
            stuff_plus = calculate_stuff_plus(pitch, batter_side)
            print(f"[{batter_side} Batter] Stuff+ Score: {stuff_plus}")
    time.sleep(2)
