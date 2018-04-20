package org.savageworlds.vtt.virtualtabletopapi.models;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;

@Entity
public class GearType {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name = "";

	private String description = "";
}
