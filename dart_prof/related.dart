import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'models.dart';

// When you have type `Map<String, List<int>>` Dart AOT compiler can't figure
// out that this container can contain only specific type of lists as
// values. However if you create an indirection (like
// `Map<String, TaggedPosts>`) that allows AOT compiler to figure out the type
// of `map[k].posts` because `TaggedPosts` is a concrete class with a single
// implementation - so its static type is enough.
class TaggedPosts {
  final List<int> posts = [];
}

void main() {
  final postsJson =
      jsonDecode(File('../posts.json').readAsStringSync()) as List<dynamic>;

  final posts = postsJson.map(Post.fromJson).toList(growable: false);

  final sw = Stopwatch()..start();

  final tagMap = <String, TaggedPosts>{};

  for (var i = 0; i < posts.length; i++) {
    final tags = posts[i].tags;
    for (var j = 0; j < tags.length; j++) {
      (tagMap[tags[j]] ??= TaggedPosts()).posts.add(i);
    }
  }

  final profIndex = sw.elapsedMicroseconds;
  var profCount = 0;
  var profSelect = 0;
  var profReset = 0;

  // preallocate and reuse
  final taggedPostCount = Uint8List(posts.length);

  final allRelatedPosts = List.generate(posts.length, (i) {
    final post = posts[i];

    final t0 = sw.elapsedMicroseconds;
    // For now simply inline fillRange which is unfortunately too slow.
    for (var i = 0; i < posts.length; i++) {
      taggedPostCount[i] = 0;
    }
    final t1 = sw.elapsedMicroseconds;

    for (var j = 0; j < post.tags.length; j++) {
      final taggedPosts = tagMap[post.tags[j]]!.posts;
      for (var k = 0; k < taggedPosts.length; k++) {
        final otherPostIdx = taggedPosts[k];
        taggedPostCount[otherPostIdx] += 1;
      }
    }

    taggedPostCount[i] = 0; // don't include self
    final t2 = sw.elapsedMicroseconds;

    // Compute max 5 without doing any unnecessary allocations.
    const topN = 5;
    final top5 = Int32List(topN * 2); // flattened list of (count, id)
    var minTags = 0;

    for (var i = 0; i < taggedPostCount.length; i++) {
      final count = taggedPostCount[i];
      if (count <= minTags) {
        continue;
      }

      // Find upper bound: pos at which count is larger than current one.
      var upperBound = (topN - 2) * 2;
      while (upperBound >= 0 && count > top5[upperBound]) {
        top5[upperBound + 2] = top5[upperBound];
        top5[upperBound + 3] = top5[upperBound + 1];
        upperBound -= 2;
      }
      final insertionPos = upperBound + 2;
      top5[insertionPos] = count;
      top5[insertionPos + 1] = i;

      minTags = top5[topN * 2 - 2];
    }

    final t3 = sw.elapsedMicroseconds;
    profReset += t1 - t0;
    profCount += t2 - t1;
    profSelect += t3 - t2;

    return {
      "_id": post.iD,
      "tags": post.tags,
      "related": [
        for (var i = 1; i < 10; i += 2) posts[top5[i]],
      ],
    };
  }, growable: false);

  print('Processing time (w/o IO): ${sw.elapsedMilliseconds}ms');
  print('PROFILE index: ${profIndex / 1000.0} ms '
      'count: ${profCount / 1000.0} ms select: ${profSelect / 1000.0} ms '
      'reset: ${profReset / 1000.0} ms');

  File('../related_posts_dart_prof.json')
      .writeAsStringSync(jsonEncode(allRelatedPosts));
}
