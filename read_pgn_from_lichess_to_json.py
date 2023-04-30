import json
import chess
import io
import chess.pgn
from collections import defaultdict
import urllib.request

# URL of the pgn file
url = "https://lichess.org/api/broadcast/lrdHUzyS.pgn"

# Read the pgn file from the url
pgn_file = urllib.request.urlopen(url)
pgn_data = io.TextIOWrapper(pgn_file, encoding='utf-8', errors='ignore')

dic = defaultdict(dict)
for nb_game in range(1,15):
    game = chess.pgn.read_game(pgn_data)
    if game is None:
        pass  # end of file
    else:
        if nb_game<15:
            type_game = "classical"
        if (nb_game>14) & (nb_game<19):
            type_game = "tiebreak rapid"
        event = game.headers.get("Event")
        white = res =  ' '.join(game.headers.get("White").split(", ")[::-1])
        black = res =  ' '.join(game.headers.get("Black").split(", ")[::-1])
        round = "Ronde {}".format(game.headers.get("Round").split(".")[0])
        if round == "Ronde ?":
            round = "Ronde {}".format(nb_game)
        winner = game.headers.get("Result")
        dic["round_{}".format(round)] = {
            "event":event, "type_game": type_game,'round':round, "round2":nb_game, "white":white, "black":black, "resultat":winner, "moves":{}
        }
        for node in game.mainline():
            

            dic["round_{}".format(round)]["moves"]["{}:{}".format(node.ply(), node.san())] =  {
                "move_number":node.ply(), "move":node.san(),
                "turn": node.turn(), 
                # "eval" : node.eval(), 
                # "clock":node.clock(),
                # "proba_win_pov_white":node.eval().wdl().white().expectation(),
                "evaluation_pov_white":node.eval().white().score(mate_score=10000) if node.eval() else 0
            }

with open("./data/games_championnat_monde_echecs_2023.json", "w") as f:
    json.dump(dic, f, indent=4)
