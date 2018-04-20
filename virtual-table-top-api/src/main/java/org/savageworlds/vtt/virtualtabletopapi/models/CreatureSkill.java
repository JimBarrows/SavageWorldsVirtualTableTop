package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotNull;
import java.util.Objects;

@Entity
public class CreatureSkill {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@OneToOne
	@NotNull
	private Skill skill;

	private Attribute attribute;

	public CreatureSkill(@NotNull final Skill skill, final Attribute attribute) {
		this.skill = skill;
		this.attribute = attribute;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof CreatureSkill)) return false;
		final CreatureSkill that = (CreatureSkill) o;
		return getId() == that.getId() &&
				getVersion() == that.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("skill", skill)
				.append("attribute", attribute)
				.toString();
	}

	public long getId() {
		return id;
	}

	public void setId(final long id) {
		this.id = id;
	}

	public long getVersion() {
		return version;
	}

	public void setVersion(final long version) {
		this.version = version;
	}

	public Skill getSkill() {
		return skill;
	}

	public void setSkill(final Skill skill) {
		this.skill = skill;
	}

	public Attribute getAttribute() {
		return attribute;
	}

	public void setAttribute(final Attribute attribute) {
		this.attribute = attribute;
	}
}
