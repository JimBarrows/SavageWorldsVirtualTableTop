package org.savageworlds.vtt.virtualtabletopapi.models;

import org.apache.commons.lang3.builder.ToStringBuilder;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import java.util.Objects;

@Entity
public class User {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false, unique = true)
	private String username;

	@NotEmpty
	@Column(nullable = false)
	private String password;

	public User() {
	}

	public User(@NotEmpty final String username, @NotEmpty final String password) {
		this.username = username;
		this.password = password;
	}

	@Override
	public int hashCode() {

		return Objects.hash(getId(), getVersion());
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) return true;
		if (!(o instanceof User)) return false;
		final User user = (User) o;
		return getId() == user.getId() &&
				getVersion() == user.getVersion();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this)
				.append("id", id)
				.append("version", version)
				.append("username", username)
				.append("password", password)
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

	public String getUsername() {
		return username;
	}

	public void setUsername(final String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(final String password) {
		this.password = password;
	}
}
