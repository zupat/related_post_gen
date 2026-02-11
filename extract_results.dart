import 'dart:io';

// script to extract benchmark results and update readme.md

final langRegex = RegExp(r'^[a-zA-Z]');
final colonOrNewLineRegex = RegExp(r'[:\n]');
final pTimeRegex = RegExp(
  r'Processing time \(w/o IO\)[^0-9]*([\d.]+)\s?(ms|s|milliseconds)',
);
final tTimeRegex = RegExp(r'Time[^0-9]*([\d.]+ (ms|s))');

const multiCoreHeading = '''
### Multicore Results

| Language       | Time (5k posts) | 20k posts        | 60k posts        | Total     |
| -------------- | --------------- | ---------------- | ---------------- | --------- | 
''';

const highlyOptimizedHeading = '''
### Highly Optimised Results

| Language       | Time (5k posts) | 20k posts        | 60k posts        | Total     |
| -------------- | --------------- | ---------------- | ---------------- | --------- | 
''';

var min5k = double.maxFinite;
var min20k = double.maxFinite;
var min60k = double.maxFinite;
var con_min5k = double.maxFinite;
var con_min20k = double.maxFinite;
var con_min60k = double.maxFinite;
var ho_min5k = double.maxFinite;
var ho_min20k = double.maxFinite;
var ho_min60k = double.maxFinite;

void main(List<String> args) {
  final filename = args.firstOrNull;

  if (filename == null) return print('Usage: extract <filename>');

  final lines = FileSystemEntity.isFileSync(filename)
      ? File(filename).readAsLinesSync()
      : Directory(filename)
            .listSync()
            .whereType<File>()
            .where((f) => f.path.endsWith('.md'))
            .expand((f) => f.readAsLinesSync())
            .toList();

  final scores = <String, List<Score>>{};

  String? currentLang;

  for (final line in lines) {
    if (langRegex.hasMatch(line)) {
      final name = line.trim().replaceAll(colonOrNewLineRegex, '');

      if (!scores.containsKey(name)) {
        scores[name] = [];
      }

      final newScore = Score(name: name);
      scores[name]!.add(newScore);
      currentLang = name;
      continue;
    }

    final processTimeMatch = pTimeRegex.firstMatch(line);

    if (processTimeMatch != null) {
      final unit = processTimeMatch
          .group(2)!
          .replaceFirst('milliseconds', 'ms');

      final time = double.parse(processTimeMatch.group(1)!.trim());

      final currentScore = scores[currentLang]!.last;
      currentScore.addTime(time, unit);

      continue;
    }
  }

  final sortedScores = scores.values.toList()
    ..sort((a, b) {
      final aSum = a.fold(0.0, (total, sc) => sc.avgTimeMS() + total);
      final bSum = b.fold(0.0, (total, sc) => sc.avgTimeMS() + total);
      return aSum.compareTo(bSum);
    });

  if (args.length > 1) {
    sortedScores.forEach(print);
  }

  final multiCoreScores = sortedScores
      .where((s) => s.first.name.contains('Concurrent'))
      .toList();

  final hoScores = sortedScores
      .where((s) => {'Julia HO', 'D HO', 'Rust HO'}.contains(s.first.name))
      .toList();

  sortedScores.removeWhere((s) => s.first.name.contains('Concurrent'));
  sortedScores.removeWhere((s) => {'Julia HO', 'D HO', 'Rust HO'}.contains(s.first.name));

  if (sortedScores.isNotEmpty && sortedScores.first.length != 3) {
    sortedScores.forEach(print);
    print(
      '${lines}\n\nEnough scores not found. Need 3 scores for each language to update readme.md - $currentLang',
    );
    return;
  }

  // caclulate min times
  min5k = sortedScores.fold(
    min5k,
    (min, sc) => sc[0].avgTimeMS() < min ? sc[0].avgTimeMS() : min,
  );
  min20k = sortedScores.fold(
    min20k,
    (min, sc) => sc[1].avgTimeMS() < min ? sc[1].avgTimeMS() : min,
  );
  min60k = sortedScores.fold(
    min60k,
    (min, sc) => sc[2].avgTimeMS() < min ? sc[2].avgTimeMS() : min,
  );
  con_min5k = multiCoreScores.fold(
    con_min5k,
    (min, sc) => sc[0].avgTimeMS() < min ? sc[0].avgTimeMS() : min,
  );
  con_min20k = multiCoreScores.fold(
    con_min20k,
    (min, sc) => sc[1].avgTimeMS() < min ? sc[1].avgTimeMS() : min,
  );
  con_min60k = multiCoreScores.fold(
    con_min60k,
    (min, sc) => sc[2].avgTimeMS() < min ? sc[2].avgTimeMS() : min,
  );
  ho_min5k = hoScores.fold(
    ho_min5k,
    (min, sc) => sc[0].avgTimeMS() < min ? sc[0].avgTimeMS() : min,
  );
  ho_min20k = hoScores.fold(
    ho_min20k,
    (min, sc) => sc[1].avgTimeMS() < min ? sc[1].avgTimeMS() : min,
  );
  ho_min60k = hoScores.fold(
    ho_min60k,
    (min, sc) => sc[2].avgTimeMS() < min ? sc[2].avgTimeMS() : min,
  );

  final parentDir = FileSystemEntity.parentOf(
    filename,
  ).split(Platform.pathSeparator)..add('readme.md');

  final readmeFile = File(parentDir.join('/'));

  if (!readmeFile.existsSync()) return print('$readmeFile not found');

  final readmeLines = readmeFile.readAsLinesSync();

  var shouldReplace = false;
  var replaced = false;

  final newReadmeContent = readmeLines
      .map((line) {
        if (line.startsWith('| -----') && !replaced && !shouldReplace) {
          shouldReplace = true;
          return line;
        }

        if (!shouldReplace) return line;

        // removes every line between the table heading and details open tag
        if (!line.trim().contains('<details>')) return null;

        shouldReplace = false;
        replaced = true;

        final sCoreLines =
            sortedScores.map((e) => e.toRowString()).join('\n') + '\n\n';
        final mCoreLines =
            multiCoreScores.map((e) => e.toRowString()).join('\n') + '\n\n';
        final hoLines =
            hoScores.map((e) => e.toRowString()).join('\n') + '\n\n';
        // final memUsageLines = sortedMemScores.map((e) => e.toRowString(true)).join('\n') + '\n\n';

        // add back the line with detail opening tag
        return sCoreLines + multiCoreHeading + mCoreLines + highlyOptimizedHeading + hoLines + line;
        // return sCoreLines + multiCoreHeading + mCoreLines + memUsageHeading + memUsageLines + line;
      })
      .whereType<String>()
      .join('\n');

  readmeFile.writeAsStringSync(newReadmeContent);
}

typedef Time = ({double time, String unit});

class Score {
  final String name;
  final List<Time> processingTimes = [];

  Score({required this.name});

  double avgTimeMS() {
    // if (processingTimes.isEmpty) throw Exception('No processing time found for $name');
    if (processingTimes.isEmpty) return double.maxFinite;

    return processingTimes.fold(0.0, (total, el) => el.millis + total) /
        processingTimes.length;
  }

  String avgTimeString() {
    final avg = avgTimeMS();

    if (avg >= double.maxFinite) return 'OOM';

    if (avg < 1000) return avgTimeMS().toStringAsFixed(2) + ' ms';

    return (avg / 1000).toStringAsFixed(2) + ' s';
  }

  void addTime(double time, String unit) {
    processingTimes.add((time: time, unit: unit));
  }

  @override
  String toString() {
    return '| $name | ${avgTimeString()}  |';
  }
}

extension on List<Score> {
  String toRowString() {
    var name = first.name;
    final originalName = name;

    if (name == 'Julia HO') {
      name = '_Julia HO_[^1]';
    } else if (name == 'D HO') {
      name = '_D HO_[^1]';
    } else if (name == 'Rust HO') {
      name = '_Rust HO_[^1]';
    } else if (name == 'Inko') {
      name = 'Inko[^2]';
    }

    final isConcurrent = name.contains('Concurrent');
    final isHO = {'Julia HO', 'D HO', 'Rust HO'}.contains(originalName);

    final min5KToUse = isConcurrent ? con_min5k : (isHO ? ho_min5k : min5k);
    final min20KToUse = isConcurrent ? con_min20k : (isHO ? ho_min20k : min20k);
    final min60KToUse = isConcurrent ? con_min60k : (isHO ? ho_min60k : min60k);

    final fiveKTime = first.avgTimeMS() == min5KToUse
        ? '\$\\textsf{\\color{lightgreen}${first.avgTimeString()}}\$'
        : first.avgTimeString();

    final twentyKTime = this[1].avgTimeMS() == min20KToUse
        ? '\$\\textsf{\\color{lightgreen}${this[1].avgTimeString()}}\$'
        : this[1].avgTimeString();

    final sixtyKTime = this[2].avgTimeMS() == min60KToUse
        ? '\$\\textsf{\\color{lightgreen}${this[2].avgTimeString()}}\$'
        : this[2].avgTimeString();

    return '| ${name} | ${fiveKTime} | ${twentyKTime} | ${sixtyKTime} | ${this.totalString} |';
  }

  String get totalString {
    if (this[2].avgTimeString() == 'OOM') return 'N/A';

    final sum = fold(0.0, (total, sc) => sc.avgTimeMS() + total);

    if (sum < 1000) return sum.toStringAsFixed(2) + ' ms';

    return (sum / 1000).toStringAsFixed(2) + ' s';
  }
}

extension on Time {
  double get millis => unit == 'ms' ? time : time * 1000;
}