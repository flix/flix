package ca.uwaterloo.flix.runtime.interpreter;

import java.util.Objects;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

/**
 * LockConditionPair contains a Condition with its matching Lock.
 * The class is immutable.
 */
public class LockConditionPair {
  /**
   * Lock for this LockConditionPair. The Lock is final.
   */
  private final Lock lock;
  /**
   * Lock for this LockConditionPair. The Lock is final.
   */
  private final Condition condition;

  /**
   * Creates a new LockConditionPair.
   *
   * @param lock      The Lock for this LockConditionPair
   * @param condition The Condition for this LockConditionPair
   */
  public LockConditionPair(Lock lock, Condition condition) {
    this.lock = lock;
    this.condition = condition;
  }

  /**
   * Gets the Lock for this LockConditionPair.
   *
   * @return Lock for this LockConditionPair
   */
  public Lock getLock() {
    return lock;
  }

  /**
   * Gets the Condition for this LockConditionPair.
   *
   * @return Condition for this LockConditionPair
   */
  public Condition getCondition() {
    return condition;
  }

  /**
   * Test this LockConditionPair for equality with another Object.
   * If the Object to be tested is not a Pair or is null, then this method returns false.
   * Two Pairs are considered equal if and only if both the Lock and Condition are equal.
   *
   * @param other the Object to test for equality with this LockConditionPair
   * @return true if the given Object is equal to this LockConditionPair else
   */
  @Override
  public boolean equals(Object other) {
    if (this == other) return true;
    if (other == null || getClass() != other.getClass()) return false;
    LockConditionPair lockConditionPair = (LockConditionPair) other;
    return lock.equals(lockConditionPair.lock) && condition.equals(lockConditionPair.condition);
  }

  /**
   * Generate a hash code for this LockConditionPair.
   * The hash code is calculated using both the Lock and the Condition of the LockConditionPair.
   *
   * @return hash code for this LockConditionPair
   */
  @Override
  public int hashCode() {
    return Objects.hash(lock, condition);
  }
}
