import matplotlib.pyplot as plt
import argparse
import csv

class Datum:
    def __init__(self, name, xs, ys):
        self.name = name
        self.xs = xs
        self.ys = ys

def plot(fs):
    for f in fs:
        plt.line(fs.xs, fs.ys, label=fs.name)

    plt.xlabel("Number of Lines of IDL")
    plt.ylabel("Number of FFTs Matched")
    plt.legend()
    plt.savefig("idl_graph.eps")

def load_files(files):
    lines = []
    for f in files:
        with open(f, "r") as dat:
            reader = csv.reader(f)
            next(reader, None) # skip headers

            xs = []
            ys = []
            for line in reader:
                xs.append(line[0])
                ys.append(line[1])
        lines.appens(Datum(f, xs, ys))
    return lines

if __name__ == "__main__":
    parser = argparse.ArugmentParser()

    parser.add_argument('DataFiles', nargs='+')

    args = praser.parse_args()

    datas = load_files(args.DataFiles)
    plot(datas)
