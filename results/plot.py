import matplotlib.pyplot as plt
import numpy as np
import argparse
import glob
import os

def parse_filename(fname):
    # format is: <input size>.json_out
    number = int(os.path.basename(fname).split(".")[0])
    return number

def load_from_file(fname):
    times = []
    with open(fname) as f:
        for line in f.readlines():
            time=int(line.strip())
            times.append(time)

    # Get a median.  TODO -- get CI.
    median = np.median(times)
    print("For file " + fname + " have median " + str(median))
    return median

def get_lines_from(set):
    lists = sorted(set.items())
    x,y = zip(*lists)

    return x, y

def plot(set1, set2):
    x, y = get_lines_from(set1)
    x2, y2 = get_lines_from(set2)

    fix, ax = plt.subplots()
    ax.set_xscale('log', basex=2)
    plt.plot(x, y, label="Original")
    plt.plot(x2, y2, label="FFTW")
    plt.ylabel("Running Time (ns)")
    plt.xlabel("Input size")
    plt.legend()
    plt.title("Speed Comparison Across Different Sizes")

    plt.savefig("output.eps")

def load_result_maps(folder):
    files = glob.glob(folder + "/*.json_out")
    results = {}

    for file in files:
        # get the number:
        number = parse_filename(file)
        values = load_from_file(file)
        results[number] = values

    return results

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="FFT Result Plotter")

    parser.add_argument("OriginalResultsFolder")
    parser.add_argument("AcceleratedResultsFolder")

    args = parser.parse_args()

    v1 = load_result_maps(args.OriginalResultsFolder)
    v2 = load_result_maps(args.AcceleratedResultsFolder)

    plot(v1, v2)
