package cspfj.util;

import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import com.google.common.collect.ImmutableMap;

public final class Util {
    private Util() {
    }

    public static <T> Map<T, Integer> indexes(List<T> list) {
        final ImmutableMap.Builder<T, Integer> builder = ImmutableMap.builder();
        for (final ListIterator<T> itr = list.listIterator(); itr.hasNext();) {
            final int index = itr.nextIndex();
            builder.put(itr.next(), index);
        }
        return builder.build();
    }

}
