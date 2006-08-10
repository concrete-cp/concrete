/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.constraint;

public final class PredicateOperations {

    private PredicateOperations() {
        super();
    }

    public static int neg(final int x) {
        return -x;
    }

    public static int abs(final int x) {
        return x >= 0 ? x : -x;
    }

    public static int add(final int x, final int y) {
        return x + y;
    }

    public static int sub(final int x, final int y) {
        return x - y;
    }

    public static int mul(final int x, final int y) {
        return x * y;
    }

    public static int div(final int x, final int y) {
        return x / y;
    }

    public static int mod(final int x, final int y) {
        return x % y;
    }

    public static int pow(final int x, final int y) {
        return (int) Math.pow(x, y);
    }

    public static int min(final int x, final int y) {
        return x < y ? x : y;
    }

    public static int max(final int x, final int y) {
        return x > y ? x : y;
    }

    public static boolean eq(final int x, final int y) {
        return x == y;
    }

    public static boolean ne(final int x, final int y) {
        return x != y;
    }

    public static boolean ge(final int x, final int y) {
        return x >= y;
    }

    public static boolean gt(final int x, final int y) {
        return x > y;
    }

    public static boolean le(final int x, final int y) {
        return x <= y;
    }

    public static boolean lt(final int x, final int y) {
        return x < y;
    }

    public static boolean not(final boolean x) {
        return true ^ x;
    }

    public static boolean and(final boolean x, final boolean y) {
        return x && y;
    }

    public static boolean or(final boolean x, final boolean y) {
        return x || y;
    }

    public static boolean xor(final boolean x, final boolean y) {
        return x ^ y;
    }

}
