import argparse
import matplotlib.pyplot as plt

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('InfoFile')
    parser.add_argument('BrokenFile')
    args = parser.parse_args()

    total_size = 0
    with open(args.InfoFile) as f:
        reasons = {}
        for line in f.readlines():
            print (line)
            reason = line.split(":")[1].strip()
            if reason in reasons:
                reasons[reason] += 1
                total_size += 1
            else:
                reasons[reason] = 1
                total_size += 1
    with open(args.BrokenFile) as f:
        for line in f.readlines():
            print(line)
            reason = line.split(":")[1].strip()
            if reason in reasons:
                reasons[reason] += 1
                total_size += 1
            else:
                reasons[reason] = 1
                total_size += 1

    sizes = []
    explode = []
    labels = []

    for key in sorted(reasons.keys()):
        if key == "Supported":
            explode.append(0.1)
        else:
            explode.append(0.0)

        sizes.append(float(reasons[key]) / float(total_size))
        labels.append(key)

    fig, ax = plt.subplots()
    ax.pie(sizes, explode=explode, labels=labels, autopct='%1.1f%%', shadow=True, startangle=90)
    ax.axis('equal')

    plt.savefig('open_source_project_distribution.eps')
