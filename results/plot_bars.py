import argparse
import matplotlib.pyplot as plt
import plot
import plot_ffta
import numpy as np

def plot_graph(fftw_results, ffta_results):
    fix, ax = plt.subplots(figsize=(8,3))
    ax.set_yscale('log', basey=10)
    xpos = np.arange(0,  len(fftw_results))
    width = 0.25
    print (ffta_results)
    print (len(ffta_results))
    print (fftw_results)
    print (len(fftw_results))
    plt.ylim([0.1, 10000])
    plt.bar(xpos, ffta_results, width, label='FFTA')
    plt.bar(xpos + width, fftw_results,  width,  label='FFTW')
    plt.legend()
    plt.xticks(xpos + width  / 2, range(0, len(fftw_results)))
    plt.xlabel('Project Number')
    plt.ylabel('Speedup')

    plt.tight_layout()
    plt.savefig('barplot_speedup.eps')

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('size', type=int)
    parser.add_argument('directories', nargs='+')
    args = parser.parse_args()

    ffta_results = []
    fftw_results = []

    i = 0
    for folder in args.directories:
        print ("Folder " + folder)
        print ("is project " + str(i))
        i += 1
        # Get the FFTW results:
        fftw_orig = plot.load_result_maps(folder, 'accelerated_fftw_orig_results', "json_out")
        fftw_acc = plot.load_result_maps(folder, 'accelerated_fftw_results', "json_out")

        if args.size not in fftw_orig:
            size = max(fftw_orig.keys())
            print("For project failed to get the sizes")
            print (folder)
            print ("Using size ", size)
        else:
            size = args.size

        fftw_results.append(fftw_orig[size] / fftw_acc[size])
        print ("Value used is ", fftw_results[-1])

        # Get the FFTA results:
        ffta_orig, _ = plot_ffta.read_file(folder + "/ffta_results/" + "UnAcceleratedResults")
        ffta_acc, _ = plot_ffta.read_file(folder + "/ffta_results/" + "AcceleratedResults")

        if args.size not in ffta_orig:
            size = max(ffta_orig.keys())
            print("For project failed to get the sizes")
            print (folder)
            print ("Using size ", size)
        else:
            size = args.size

        ffta_results.append(np.median(ffta_orig[size] / ffta_acc[size]))
        print ("Value used is ", ffta_results[-1])

    plot_graph(fftw_results, ffta_results)
