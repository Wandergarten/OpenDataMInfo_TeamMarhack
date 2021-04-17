import altair as alt
import pandas as pd


dt = pd.read_csv("./Data/game_simulation_data.csv")
dt = dt.convert_dtypes(infer_objects=True)
print(dt.dtypes)
# Aggregationslevel


# subsets der Spiele
dt['player1_wins'] = (dt[['player1', 'player2', 'game_id']].groupby(by='game_id').max()).player1 == 21

## Spielerprofil & Spielerdaten

# wie kriege ich alle Spiele in denen Chris mitgespielt hat in eine Tabelle?
# subsetting nach player1_id ODER player2_id (weil Chris kann ja p1 oder p2 sein)
# python code:

# print(dt[(dt['player1_id'] == "Chris") | (dt['player2_id'] == "Chris")])
chris_data = dt[(dt['player1_id'] == "Chris") | (dt['player2_id'] == "Chris")]
# jetzt müssen wir unsere Statistiken / Plots basteln für chris. und chris können wir dann später zb. durch ein dropdown mit allen
# unique values in der player1_id spalte ersetzen.
# okay .. mit altair jetzT?
# jo würde ich sagen. das geht echt ziemlich flott und einfach.
# ok dann schau ich mir mal die doku an
# du kannst auch unten das subsetting nach game_id vorbereiten.

# alt.Chart(chris_data).mark_line(

# )

# welchen plot brauchen wir denn als erstes?

# Match-Analyse

# subsetting nach game_id


# Nutzerdatenanalyse aus der Vogelperspektive

# quasi kein subsetting
