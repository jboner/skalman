/**
 * Copyright (C) 2009 Scalable Solutions.
 */

package se.scalablesolutions.skalman.annotation;

import java.lang.annotation.*;

// FIXME: Should be configurable with the number of retries
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface RetryOnError {}