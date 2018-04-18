package org.savageworlds.vtt.virtualtabletopapi.models;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.LinkedHashSet;
import java.util.Set;

@Entity
public class Story {

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private long id;

	@Version
	private long version;

	@NotEmpty
	@Column(nullable = false)
	private String name;

	@NotEmpty
	private String description;

	@ManyToOne
	@NotNull
	private PlotPoint plotPoint;

	@ManyToOne
	@NotNull
	private Location startingLocation;

	@OneToMany
	@OrderBy("sequence")
	private Set<Scene> scenes = new LinkedHashSet<>();

}
