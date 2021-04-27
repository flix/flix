package ca.uwaterloo.flix.runtime.interpreter;

import java.util.Objects;

class ChannelIndexPair {
    private final Channel channel;
    private final int index;

    ChannelIndexPair(Channel channel, int index) {
        this.channel = channel;
        this.index = index;
    }

    public Channel getChannel() {
        return channel;
    }

    public int getIndex() {
        return index;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ChannelIndexPair that = (ChannelIndexPair) o;
        return index == that.index && Objects.equals(channel, that.channel);
    }

    @Override
    public int hashCode() {
        return Objects.hash(channel, index);
    }

    @Override
    public String toString() {
        return "ChannelIndexPair{" +
                "channel=" + channel +
                ", index=" + index +
                '}';
    }
}
