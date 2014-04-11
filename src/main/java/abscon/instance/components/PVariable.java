package abscon.instance.components;


public class PVariable {
	private String name;

	private PDomain domain;

	public String getName() {
		return name;
	}

	public PDomain getDomain() {
		return domain;
	}

	public PVariable(String name, PDomain domain) {
		this.name = name;
		this.domain = domain;
	}

	public String toString() {
		return "  variable " + name + " with associated domain " + domain.getName();
	}
}
