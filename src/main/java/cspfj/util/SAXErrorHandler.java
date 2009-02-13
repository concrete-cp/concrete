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

package cspfj.util;

import org.xml.sax.helpers.DefaultHandler;

import java.text.*;
import java.util.logging.Logger;

import org.xml.sax.*;

public class SAXErrorHandler extends DefaultHandler {
    private static final Logger logger = Logger
            .getLogger("cspfj.generator.ErrorHandler");

    private void print(final SAXParseException exception) {
        final String msg = new MessageFormat("({0}: {1}, {2}): {3}")
                .format(new Object[] { exception.getSystemId(),
                        Integer.valueOf(exception.getLineNumber()),
                        Integer.valueOf(exception.getColumnNumber()),
                        exception.getMessage() });
        logger.warning(msg);
    }

    public void warning(final SAXParseException exception) {
        print(exception);
    }

    public void error(final SAXParseException exception) {
        print(exception);
    }

    public void fatalError(final SAXParseException exception)
            throws SAXParseException {
        print(exception);
        throw exception;
    }
}
